{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Nuchain.Consensus.Util
  ( resetElectionTimer
  , resetElectionTimerLeader
  , resetHeartbeatTimer
  , hasElectionTimerLeaderFired
  , cancelTimer
  , becomeFollower
  , queryLogs
  , updateLogs
  , debug
  , randomRIO
  , runRWS_
  , enqueueEvent
  , dequeueEvent
  , logMetric
  , logStaticMetrics
  , setTerm
  , setRole
  , setCurrentLeader
  , enqueueRequest
  , enqueueRequest'
  , sendHistoryNewKeys
  , queryHistoryForExisting
  , queryHistoryForPriorApplication
  , now
  ) where

import Control.Exception.Lifted (mask_)
import Control.Lens hiding (Index)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.RWS.Strict
import Control.Concurrent (putMVar, takeMVar, newEmptyMVar)

import Data.HashSet (HashSet)
import Data.Set (Set)
import Data.Map.Strict (Map)
import Data.Thyme.Clock
import qualified System.Random as R

import qualified Nuchain.Config.ClusterMembership as CM
import Nuchain.Config.TMVar
import Nuchain.Types
import qualified Nuchain.Types.Sender as Sender
import qualified Nuchain.Log.Types as Log
import qualified Nuchain.Types.Log as Log
import qualified Nuchain.Types.History as History
import Nuchain.Util.Util

getNewElectionTimeout :: Consensus Int
getNewElectionTimeout = do
  viewConfig electionTimeoutRange >>= randomRIO

resetElectionTimer :: Consensus ()
resetElectionTimer = do
  theCfg <- readConfig
  let theNode = _alias (_nodeId theCfg)
  timeout <- getNewElectionTimeout
  debug $ (show theNode) ++ ": Resetting Election Timer - setting a new timed event for "
    ++ show (timeout `div` 1000) ++ "ms"
  setTimedEvent (ElectionTimeout $ show (timeout `div` 1000) ++ "ms") timeout

-- | If a leader hasn't heard from a follower in longer than 2x max election timeouts, he should
--   step down.
hasElectionTimerLeaderFired :: Consensus Bool
hasElectionTimerLeaderFired = do
  maxTimeout <- ((*2) . snd) <$> viewConfig electionTimeoutRange
  timeSinceLastAER' <- use csTimeSinceLastAER
  let leaderTimeout = timeSinceLastAER' >= maxTimeout
  theConfig <- readConfig
  when leaderTimeout $
    throwDiagnostics (_enableDiagnostics theConfig) "The leader has timed out"
  return leaderTimeout

resetElectionTimerLeader :: Consensus ()
resetElectionTimerLeader = csTimeSinceLastAER .= 0

resetHeartbeatTimer :: Consensus ()
resetHeartbeatTimer = do
  timeout <- viewConfig heartbeatTimeout
  debug $ ": Resetting Heartbeat Timer - setting a new timed event for "
    ++ show (timeout `div` 1000) ++ "ms"
  setTimedEvent (HeartbeatTimeout $ show (timeout `div` 1000) ++ "ms") timeout

cancelTimer :: Consensus ()
cancelTimer = do
  asy <- use csTimerKey
  case asy of
    Nothing -> return ()
    Just a -> view killEnqueued >>= \f -> mask_ $ do
      liftIO $ f a
      csTimerKey .= Nothing

setTimedEvent :: Event -> Int -> Consensus ()
setTimedEvent e t = do
  cancelTimer
  view enqueueLater >>= \f -> mask_ $ do
    tk <- liftIO $ f t e
    csTimerKey .= Just tk

becomeFollower :: Consensus ()
becomeFollower = do
  debug "becoming follower"
  setRole Follower
  resetElectionTimer

queryLogs :: Set Log.AtomicQuery -> Consensus (Map Log.AtomicQuery Log.QueryResult)
queryLogs q = do
  enqueueLogQuery' <- view enqueueLogQuery
  mv <- liftIO newEmptyMVar
  liftIO . enqueueLogQuery' $! Log.Query q mv
  liftIO $! takeMVar mv

updateLogs :: UpdateLogs -> Consensus ()
updateLogs q = do
  enqueueLogQuery' <- view enqueueLogQuery
  liftIO . enqueueLogQuery' $! Log.Update q

debug :: String -> Consensus ()
debug s = do
  when (not (null s)) $ do   
    dbg <- view (rs.debugPrint)
    role' <- use csNodeRole
    case role' of
      Leader -> liftIO $! dbg $! "[Nuchain|\ESC[0;34mLEADER\ESC[0m]: " ++ s
      Follower -> liftIO $! dbg $! "[Nuchain|\ESC[0;32mFOLLOWER\ESC[0m]: " ++ s
      Candidate -> liftIO $! dbg $! "[Nuchain|\ESC[1;33mCANDIDATE\ESC[0m]: " ++ s

randomRIO :: R.Random a => (a,a) -> Consensus a
randomRIO rng = view (rs.random) >>= \f -> liftIO $! f rng -- R.randomRIO


-- TODO: refactor this so that sender service can directly query for the state it needs
enqueueRequest :: Sender.ServiceRequest -> Consensus ()
enqueueRequest s = do
  sendMsg <- view sendMessage
  conf <- readConfig
  st <- get
  ss <- return $! Sender.StateSnapshot
    { Sender._snapNodeId = conf ^. nodeId
    , Sender._snapNodeRole = st ^. csNodeRole
    , Sender._snapClusterMembers = conf ^. clusterMembers
    , Sender._snapLeader = st ^. csCurrentLeader
    , Sender._snapTerm = st ^. csTerm
    , Sender._snapPublicKey = conf ^. myPublicKey
    , Sender._snapPrivateKey = conf ^. myPrivateKey
    , Sender._snapYesVotes = st ^. csYesVotes
    }
  liftIO $! sendMsg $! Sender.ServiceRequest' ss s

enqueueRequest' :: Sender.ServiceRequest' -> Consensus ()
enqueueRequest' s = do
  sendMsg <- view sendMessage
  liftIO $! sendMsg s

-- no state update
enqueueEvent :: Event -> Consensus ()
enqueueEvent event = view enqueue >>= \f -> liftIO $! f event

-- no state update
dequeueEvent :: Consensus Event
dequeueEvent = view dequeue >>= \f -> liftIO f

logMetric :: Metric -> Consensus ()
logMetric metric = view (rs.publishMetric) >>= \f -> liftIO $! f metric

logStaticMetrics :: Consensus ()
logStaticMetrics = do
  Config{..} <- readConfig
  logMetric . MetricNodeId =<< viewConfig nodeId
  logMetric $ MetricClusterSize (1 + CM.countOthers _clusterMembers)
  logMetric . MetricQuorumSize $ CM.minQuorumOthers _clusterMembers
  logMetric $ MetricChangeToClusterSize (1 + CM.countTransitional _clusterMembers)
  logMetric . MetricChangeToQuorumSize $ CM.minQuorumTransitional _clusterMembers
  logMetric $ MetricClusterMembers (CM.othersAsText _clusterMembers)

-- NB: Yes, the strictness here is probably overkill, but this used to leak the bloom filter
publishConsensus :: Consensus ()
publishConsensus = do
  !currentLeader' <- use csCurrentLeader
  !nodeRole' <- use csNodeRole
  !term' <- use csTerm
  !cYesVotes' <- use csYesVotes
  p <- view mPubConsensus
  newPubCons <- return $! PublishedConsensus currentLeader' nodeRole' term' cYesVotes'
  _ <- liftIO $! takeMVar p
  liftIO $! putMVar p $! newPubCons

setTerm :: Term -> Consensus ()
setTerm t = do
  csTerm .= t
  publishConsensus
  logMetric $! MetricTerm t

setRole :: Role -> Consensus ()
setRole newRole = do
  csNodeRole .= newRole
  publishConsensus
  logMetric $! MetricRole newRole

setCurrentLeader :: Maybe NodeId -> Consensus ()
setCurrentLeader mNode = do
  csCurrentLeader .= mNode
  publishConsensus
  logMetric $! MetricCurrentLeader mNode

runRWS_ :: MonadIO m => RWST r w s m a -> r -> s -> m ()
runRWS_ ma r s = void $! runRWST ma r s

sendHistoryNewKeys :: HashSet RequestKey -> Consensus ()
sendHistoryNewKeys srks = do
  send <- view enqueueHistoryQuery
  liftIO $ send $ History.AddNew srks

queryHistoryForExisting :: HashSet RequestKey -> Consensus (HashSet RequestKey)
queryHistoryForExisting srks = do
  send <- view enqueueHistoryQuery
  m <- liftIO $ newEmptyMVar
  liftIO $ send $ History.QueryForExistence (srks,m)
  History.ExistenceResult{..} <- liftIO $ takeMVar m
  return rksThatAlreadyExist

queryHistoryForPriorApplication :: HashSet RequestKey -> Consensus (HashSet RequestKey)
queryHistoryForPriorApplication srks = do
  send <- view enqueueHistoryQuery
  m <- liftIO $ newEmptyMVar
  liftIO $ send $ History.QueryForPriorApplication (srks,m)
  History.ExistenceResult{..} <- liftIO $ takeMVar m
  return rksThatAlreadyExist

now :: Consensus UTCTime
now = view (rs.getTimestamp) >>= liftIO
