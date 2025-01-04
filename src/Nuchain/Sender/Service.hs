{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Nuchain.Sender.Service
  ( SenderService
  , ServiceEnv(..), debugPrint, serviceRequestChan, outboundGeneral
  , logService, getEvidenceState, publishMetric, aeReplicationLogLimit
  , runSenderService
  , createAppendEntriesResponse' -- we need this for AER Evidence
  , willBroadcastAE
  ) where

import Control.Concurrent
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.RWS.Lazy
import Control.Parallel.Strategies
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Serialize hiding (get, put)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Thyme.Clock (UTCTime, getCurrentTime)

import Nuchain.Types.Base
import Nuchain.Types.Evidence
import Nuchain.Event (pprintBeat)
import Nuchain.Log.Types (LogIndex(..), LogServiceChannel)
import qualified Nuchain.Log.Types as Log
import Nuchain.Message
import Nuchain.Types.Sender
import Nuchain.Config.TMVar as Cfg
import qualified Nuchain.Config.ClusterMembership as CM
import Nuchain.Types.Comms
import Nuchain.Types.Dispatch (Dispatch(..))
import qualified Nuchain.Types.Dispatch as KD
import Nuchain.Types.Log (LogEntries(..))
import qualified Nuchain.Types.Log as Log
import Nuchain.Types.Message
import Nuchain.Types.Metric (Metric)
import qualified Nuchain.Types.Spec as Spec

data ServiceEnv = ServiceEnv
  { _debugPrint :: !(String -> IO ())
  , _aeReplicationLogLimit :: Int
  -- Comm Channels
  , _serviceRequestChan :: !SenderServiceChannel
  , _outboundGeneral :: !OutboundGeneralChannel
  -- Log Storage
  , _logService :: !LogServiceChannel
  -- Evidence Thread's Published State
  , _getEvidenceState :: !(MVar PublishedEvidenceState)
  , _publishMetric :: !(Metric -> IO ())
  , _config :: !Cfg.GlobalConfigTMVar
  , _pubCons :: !(MVar Spec.PublishedConsensus)
  }
makeLenses ''ServiceEnv

type SenderService s = RWST ServiceEnv () s IO

-- TODO use configUpdater to populate an MVar local to the RWST
-- vs hitting the TVar every time we need to send something (or have SenderService do this internally)
getStateSnapshot :: Cfg.GlobalConfigTMVar -> MVar Spec.PublishedConsensus -> IO StateSnapshot
getStateSnapshot conf' pcons' = do
  conf <- Cfg.readCurrentConfig conf'
  st <- readMVar pcons'
  return $! StateSnapshot
    { _snapNodeId = Cfg._nodeId conf
    , _snapNodeRole = st ^. Spec.pcRole
    , _snapClusterMembers = Cfg._clusterMembers conf
    , _snapLeader = st ^. Spec.pcLeader
    , _snapTerm = st ^. Spec.pcTerm
    , _snapPublicKey = Cfg._myPublicKey conf
    , _snapPrivateKey = Cfg._myPrivateKey conf
    , _snapYesVotes = st ^. Spec.pcYesVotes
    }

runSenderService
  :: Dispatch
  -> Cfg.GlobalConfigTMVar
  -> (String -> IO ())
  -> (Metric -> IO ())
  -> MVar PublishedEvidenceState
  -> MVar Spec.PublishedConsensus
  -> IO ()
runSenderService dispatch gcm debugFn publishMetric' mPubEvState mPubCons = do
  conf <- Cfg.readCurrentConfig gcm
  s <- return $ StateSnapshot
    { _snapNodeId = Cfg._nodeId conf
    , _snapNodeRole = Follower
    , _snapClusterMembers = Cfg._clusterMembers conf
    , _snapLeader = Nothing
    , _snapTerm = startTerm
    , _snapPublicKey = Cfg._myPublicKey conf
    , _snapPrivateKey = Cfg._myPrivateKey conf
    , _snapYesVotes = Set.empty
    }
  env <- return $ ServiceEnv
    { _debugPrint = debugFn
    , _aeReplicationLogLimit = Cfg._aeBatchSize conf
    -- Comm Channels
    , _serviceRequestChan = _dispSenderService dispatch
    , _outboundGeneral = dispatch ^. KD.dispOutboundGeneral
    -- Log Storage
    , _logService = dispatch ^. KD.dispLogService
    , _getEvidenceState = mPubEvState
    , _publishMetric = publishMetric'
    , _config = gcm
    , _pubCons = mPubCons
    }
  void $ liftIO $ runRWST serviceRequests env s

snapshotStateExternal :: SenderService StateSnapshot ()
snapshotStateExternal = do
  mPubCons <- view pubCons
  conf <- view config
  snap <- liftIO $ getStateSnapshot conf mPubCons
  put snap
  return ()

serviceRequests :: SenderService StateSnapshot ()
serviceRequests = do
  rrc <- view serviceRequestChan
  debug "launch!"
  forever $ do
    sr <- liftIO $ readComm rrc
    case sr of
      SendAllAppendEntriesResponse{..} -> do
        snapshotStateExternal
        stTime <- liftIO $ getCurrentTime
        sendAllAppendEntriesResponse' (Just _srIssuedTime) stTime _srLastLogHash _srMaxIndex

      ForwardCommandToLeader{..} -> do
        snapshotStateExternal
        s <- get
        let ldr = view snapLeader s
        case ldr of
          Nothing -> debug $ "Leader is down, unable to forward commands. Dropping..."
          Just ldr' -> do
            debug $ "fowarding " ++ show (length $ _newCmd $ _srCommands) ++ "commands to leader"
            sendRPC ldr' $ NEW' _srCommands
      (ServiceRequest' ss m) -> do
        put ss
        case m of
          BroadcastAE{..} -> do
            mEvState <- view getEvidenceState
            evState <- liftIO $ readMVar mEvState
            sendAllAppendEntries (_pesNodeStates evState) (_pesConvincedNodes evState) _srAeBoardcastControl
          EstablishDominance -> establishDominance
          SingleAER{..} -> sendAppendEntriesResponse _srFor _srSuccess _srConvinced
          BroadcastAER -> sendAllAppendEntriesResponse
          BroadcastRV rv -> sendAllRequestVotes rv
          BroadcastRVR{..} -> sendRequestVoteResponse _srCandidate _srHeardFromLeader _srVote
      SenderBeat t -> do
        gCfg <- view config
        conf <- liftIO $ Cfg.readCurrentConfig gCfg
        liftIO (pprintBeat t conf) >>= debug

queryLogs :: Set Log.AtomicQuery -> SenderService StateSnapshot (Map Log.AtomicQuery Log.QueryResult)
queryLogs q = do
  ls <- view logService
  mv <- liftIO newEmptyMVar
  liftIO . writeComm ls $ Log.Query q mv
  liftIO $ takeMVar mv  -- blocks until mv is no longer empty

debug :: String -> SenderService StateSnapshot ()
debug s = do
  unless (null s) $ do
    dbg <- view debugPrint
    liftIO $ dbg $ "[Service|Sender] " ++ s

-- views state, but does not update
sendAllRequestVotes :: RequestVote -> SenderService StateSnapshot ()
sendAllRequestVotes rv = pubRPC $ RV' rv

createAppendEntries' :: NodeId
                     -> (LogIndex, Term, LogEntries)
                     -> Term
                     -> NodeId
                     -> Set NodeId
                     -> Set RequestVoteResponse
                     -> RPC
createAppendEntries' target (pli, plt, es) ct myNodeId' vts yesVotes' =
  let vts' = if Set.member target vts then Set.empty else yesVotes'
  in AE' $ AppendEntries ct myNodeId' pli plt es vts' NewMsg

establishDominance :: SenderService StateSnapshot ()
establishDominance = do
  debug "establishing general dominance"
  stTime <- liftIO $ getCurrentTime
  s <- get
  let ct = view snapTerm s
  let myNodeId' = view snapNodeId s
  let yesVotes' = view snapYesVotes s
  mv <- queryLogs $ Set.fromList [Log.GetMaxIndex, Log.GetLastLogTerm]
  pli <- return $! Log.hasQueryResult Log.MaxIndex mv
  plt <- return $! Log.hasQueryResult Log.LastLogTerm mv
  rpc <- return $! AE' $ AppendEntries ct myNodeId' pli plt Log.lesEmpty yesVotes' NewMsg
  edTime <- liftIO $ getCurrentTime
  pubRPC rpc
  debug $ "asserted dominance: " ++ show (interval stTime edTime) ++ "mics"

-- | Send all append entries is only needed in special circumstances. Either we have a Heartbeat
--   event or we are getting a quick win in with CMD's
sendAllAppendEntries :: Map NodeId (LogIndex, UTCTime) -> Set NodeId
                     -> AEBroadcastControl -> SenderService StateSnapshot ()
sendAllAppendEntries nodeCurrentIndex' nodesThatFollow' sendIfOutOfSync = do
  startTime' <- liftIO $ getCurrentTime
  s <- get
  let ct = view snapTerm s
  let myId' = view snapNodeId s
  let yesVotes' = view snapYesVotes s
  let cm = _snapClusterMembers s
  let allNodes = CM.getAllExcluding cm myId'
  limit' <- view aeReplicationLogLimit
  inSync' <- canBroadcastAE cm nodeCurrentIndex' ct myId' nodesThatFollow' sendIfOutOfSync
  synTime' <- liftIO $ getCurrentTime
  case inSync' of
    BackStreet{..} ->
      case broadcastRPC of
        Just broadcastableRPC -> do -- NB: SendAERegardless gets you here
          -- We can't take the short cut but the AE (which is overloaded as a heartbeat grr...)
          -- still need to be sent. This usually takes place when we hit a heartbeat timeout
          debug "followers are out of sync, publishing latest LogEntries"
          resultMap <- queryLogs $ Set.map (\n -> do
                                              let mPair = Map.lookup n nodeCurrentIndex'
                                              let mIndex = ((+) 1 . fst <$> mPair)
                                              Log.GetInfoAndEntriesAfter mIndex limit')
                                           allNodes
          rpcs <- return $
            (\target ->
               let mPair = Map.lookup target nodeCurrentIndex'
                   mIndex = ((+) 1 . fst <$> mPair)
                   ieAfter = Log.InfoAndEntriesAfter mIndex limit'
                   triple = Log.hasQueryResult ieAfter resultMap
                   entries' = createAppendEntries' target triple ct myId' nodesThatFollow' yesVotes'
               in (target, entries'))
            <$> Set.toList laggingFollowers
          endTime' <- liftIO $ getCurrentTime
          debug $ "AE servicing lagging nodes, taking " ++ printInterval startTime' endTime'
               ++ " to create (syncTime=" ++ printInterval startTime' synTime' ++ ")"
          sendRpcsPeicewise rpcs (length rpcs, endTime')
          debug $ "AE sent Regardless"
          pubRPC broadcastableRPC -- TODO: this is terrible as laggers will need a pause to catch up
                                  -- correctly unless we have them cache future AE
        Nothing -> -- NB: OnlySendIfInSync gets you here
          -- We can't just spam AE's to the followers because they can get clogged with
          -- overlapping/redundant AE's. This eventually trips an election.
          -- TODO: figure out how an out of date follower can precache LEs that it can't add to it's
          -- log yet (without tripping an election)
          debug $ "AE withheld, followers are out of sync (synTime=" ++ printInterval startTime' synTime' ++ ")"
    InSync{..} -> do
      -- Hell yeah, we can just broadcast. We don't care about the Broadcast control if we know we
      -- can broadcast. This saves us a lot of time when node count grows.
      pubRPC $ broadcastableRPC
      endTime' <- liftIO $ getCurrentTime
      debug $ "AE broadcasted, followers are in sync" ++ printInterval startTime' endTime'
        ++ " (synTime=" ++ printInterval startTime' synTime' ++ ")"

data InSync =
  InSync { broadcastableRPC :: !RPC}
  | BackStreet
    { broadcastRPC :: !(Maybe RPC)
    , laggingFollowers :: !(Set NodeId) }
  deriving (Show, Eq)

willBroadcastAE :: CM.ClusterMembership -> Map NodeId (LogIndex, UTCTime) -> Set NodeId -> NodeId -> Bool
willBroadcastAE cm nodeIndexMap votes myNodeId =
  -- we only want to do this if we know that every node is in sync with us (the leader)
  let everyoneBelieves = CM.containsAllNodesExcluding cm votes myNodeId
  in everyoneBelieves && (inSync cm nodeIndexMap myNodeId)

canBroadcastAE :: CM.ClusterMembership
               -> Map NodeId (LogIndex, UTCTime)
               -> Term
               -> NodeId
               -> Set NodeId
               -> AEBroadcastControl
               -> SenderService StateSnapshot InSync
canBroadcastAE cm nodeIndexMap ct myNodeId' vts broadcastControl =
  -- we only want to do this if we know that every node is in sync with us (the leader)
  let
    everyoneBelieves = CM.containsAllNodesExcluding cm vts myNodeId'
    mniList = fst <$> Map.elems nodeIndexMap -- get a list of where everyone is (in ascend. order of keys)
    mniSet = Set.fromList $ mniList -- condense every Followers LI into a set (in ascending order of indexes)
    latestFollower = head $ Set.toDescList mniSet
    laggingFollowers = Map.keysSet $ Map.filter ((/=) latestFollower . fst) nodeIndexMap
    elemSet = Set.elems mniSet -- indexes in ascending order
    mni = head elemSet -- (least index) totally unsafe but we only call it if we are going to broadcast
  in
    if everyoneBelieves && (inSync cm nodeIndexMap myNodeId')
    then do
      limit' <- view aeReplicationLogLimit
      mv <- queryLogs $ Set.singleton $ Log.GetInfoAndEntriesAfter (Just $ 1 + mni) limit'
      (pli,plt, es) <- return $ Log.hasQueryResult (Log.InfoAndEntriesAfter (Just $ 1 + mni) limit') mv
      debug "canBroadcastAE - InSync"
      return $ InSync $ AE' $ AppendEntries ct myNodeId' pli plt es Set.empty NewMsg
    else do
      inSyncRpc <- case broadcastControl of
        OnlySendIfFollowersAreInSync -> return Nothing
        SendAERegardless -> do
          limit' <- view aeReplicationLogLimit
          mv <- queryLogs $ Set.singleton $ Log.GetInfoAndEntriesAfter (Just $ 1 + latestFollower) limit'
          (pli,plt, es) <- return $ Log.hasQueryResult (Log.InfoAndEntriesAfter (Just $ 1 + latestFollower) limit') mv
--        debug $ "InfoAndEntriesAfter Backstreet " ++ (show (Just $ 1 + latestFollower)) ++ " " ++ show limit'
--              ++ " with results " ++ show (Log.lesMinIndex es, Log.lesMaxIndex es)
          return $! Just $! AE' $ AppendEntries ct myNodeId' pli plt es Set.empty NewMsg
      if everyoneBelieves
      then do
        debug $ "canBroadcastAE - Backsteet -"
              ++ "\n\tlaggingFollowers: " ++ show laggingFollowers
        return $ BackStreet inSyncRpc laggingFollowers
      else do
        s <- get
        let allNodes = CM.getAllExcluding (_snapClusterMembers s) myNodeId'
        let diff = allNodes Set.\\ vts
        let theUnion = Set.union laggingFollowers diff
        debug $ "canBroadcastAE - non-believers exist, establishing dominance"
        debug $ "canBroadcastAE - Backstreet - "
              ++ "\n\ttheUnion (lagging followers): " ++ show theUnion
        return $ BackStreet inSyncRpc theUnion
{-# INLINE canBroadcastAE #-}

inSync :: CM.ClusterMembership -> Map NodeId (LogIndex, UTCTime) -> NodeId -> Bool
inSync cm nodeIndexMap myNodeId =
  -- for the purpose of this calculation, ignore any evidence that may have come from nodes no longer in the configuration
  let allNodes = CM.getAllExcluding cm myNodeId
      filteredMap = Map.filterWithKey (\k _ -> k `Set.member` allNodes) nodeIndexMap
      indexSet = Set.fromList $ fst <$> Map.elems filteredMap -- get a list of where everyone is
      nodeKeys = Set.fromList $ Map.keys filteredMap
      ok = Set.size indexSet == 1
           && CM.containsAllNodesExcluding cm nodeKeys myNodeId
  in ok
{-# INLINE inSync #-}

createAppendEntriesResponse' :: Bool -> Bool -> Term -> NodeId -> LogIndex -> Hash -> RPC
createAppendEntriesResponse' success convinced ct myNodeId' lindex lhash =
  AER' $ AppendEntriesResponse ct myNodeId' success convinced lindex lhash NewMsg

-- this only gets used when a Follower is replying in the negative to the Leader
sendAppendEntriesResponse :: NodeId -> Bool -> Bool -> SenderService StateSnapshot ()
sendAppendEntriesResponse target success convinced = do
  s <- get
  let ct = view snapTerm s
  let myNodeId' = view snapNodeId s
  mv <- queryLogs $ Set.fromList [Log.GetMaxIndex, Log.GetLastLogHash]
  maxIndex' <- return $ Log.hasQueryResult Log.MaxIndex mv
  lastLogHash' <- return $ Log.hasQueryResult Log.LastLogHash mv
  sendRPC target $ createAppendEntriesResponse' success convinced ct myNodeId' maxIndex' lastLogHash'
  debug $ "sent AppendEntriesResponse: " ++ show ct

sendAllAppendEntriesResponse' :: Maybe UTCTime -> UTCTime -> LogIndex -> Hash -> SenderService StateSnapshot ()
sendAllAppendEntriesResponse' issueTime stTime maxIndex' lastLogHash' = do
  s <- get
  let ct = view snapTerm s
  let myNodeId' = view snapNodeId s
  aer <- return $ createAppendEntriesResponse' True True ct myNodeId' maxIndex' lastLogHash'
  pubRPC aer
  edTime <- liftIO $ getCurrentTime
  case issueTime of
    Nothing -> debug $ "pub AER taking " ++ show (interval stTime edTime) ++ "mics to construct"
    Just issueTime' -> debug $ "pub AER taking " ++ printInterval stTime edTime
                                ++ "(issueTime=" ++ printInterval issueTime' edTime ++ ")"

-- this is used for distributed evidence + updating the Leader with nodeCurrentIndex
sendAllAppendEntriesResponse :: SenderService StateSnapshot ()
sendAllAppendEntriesResponse = do
  stTime <- liftIO $ getCurrentTime
  mv <- queryLogs $ Set.fromList [Log.GetMaxIndex, Log.GetLastLogHash]
  maxIndex' <- return $ Log.hasQueryResult Log.MaxIndex mv
  lastLogHash' <- return $ Log.hasQueryResult Log.LastLogHash mv
  sendAllAppendEntriesResponse' Nothing stTime maxIndex' lastLogHash'

sendRequestVoteResponse :: NodeId -> Maybe HeardFromLeader -> Bool -> SenderService StateSnapshot ()
sendRequestVoteResponse target heardFromLeader vote = do
  s <- get
  let term' = view snapTerm s
  let myNodeId' = view snapNodeId s
  pubRPC $! RVR' $! RequestVoteResponse term' heardFromLeader myNodeId' vote target NewMsg

pubRPC :: RPC -> SenderService StateSnapshot ()
pubRPC rpc = do
  oChan <- view outboundGeneral
  s <- get
  let myNodeId' = view snapNodeId s
  let privKey = view snapPrivateKey s
  let pubKey = view snapPublicKey s
  sRpc <- return $ rpcToSignedRPC myNodeId' pubKey privKey rpc
  debug $ "broadcast msg sent: "
        ++ show (_digType $ _sigDigest sRpc)
        ++ (case rpc of
              AER' v -> " for " ++ show (_aerIndex v, _aerTerm v)
              RV' v -> " for " ++ show (_rvTerm v, _rvLastLogIndex v)
              RVR' v -> " for " ++ show (_rvrTerm v, _voteGranted v)
              _ -> "")
  liftIO $ writeComm oChan $ broadcastMsg [encode $ sRpc]

sendRPC :: NodeId -> RPC -> SenderService StateSnapshot ()
sendRPC target rpc = do
  oChan <- view outboundGeneral
  s <- get
  let myNodeId' = view snapNodeId s
  let privKey = view snapPrivateKey s
  let pubKey = view snapPublicKey s
  sRpc <- return $ rpcToSignedRPC myNodeId' pubKey privKey rpc
  debug $ "issuing direct msg: " ++ show (_digType $ _sigDigest sRpc) ++ " to " ++ show (unAlias $ _alias target)
  liftIO $! writeComm oChan $! directMsg [(target, encode $ sRpc)]

sendRpcsPeicewise :: [(NodeId, RPC)] -> (Int, UTCTime) -> SenderService StateSnapshot ()
sendRpcsPeicewise rpcs d@(total, stTime) = do
  (aFewRpcs,rest) <- return $! splitAt 8 rpcs
  if null rest
  then do
    sendRPCs aFewRpcs
    edTime <- liftIO $ getCurrentTime
    debug $ "Sent " ++ show total ++ " RPCs taking " ++ show (interval stTime edTime) ++ "mics to construct"
  else sendRpcsPeicewise rest d

sendRPCs :: [(NodeId, RPC)] -> SenderService StateSnapshot ()
sendRPCs rpcs = do
  oChan <- view outboundGeneral
  s <- get
  let myNodeId' = view snapNodeId s
  let privKey = view snapPrivateKey s
  let pubKey = view snapPublicKey s
  msgs <- return (((\(n,msg) -> (n, encode $ rpcToSignedRPC myNodeId' pubKey privKey msg)) <$> rpcs) `using` parList rseq)
  liftIO $ writeComm oChan $! directMsg msgs
