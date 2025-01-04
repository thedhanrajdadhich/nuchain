{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Nuchain.Execution.Service
  ( initExecutionEnv
  , runExecutionService
  ) where

import Control.Lens hiding (Index)
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.RWS.Strict
import Data.Serialize (decode)

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Strict as Map
import Data.Thyme.Clock
import Data.Tuple.Strict (T2(..))
import Data.Maybe (fromJust)
import Data.ByteString (ByteString)

import qualified Pact.Types.Command as Pact
import qualified Pact.Types.Hash as Pact
import Pact.Types.Logger (LogRules(..),initLoggers,doLog)
import qualified Pact.Types.Persistence as Pact

import Nuchain.Util.Util (linkAsyncTrack)
import Nuchain.Config
import Nuchain.Config.TMVar as Cfg
import Nuchain.Types.PactDB
import Nuchain.Types.Base
import Nuchain.Types.Execution
import Nuchain.Types.Command
import Nuchain.Types.Config
import Nuchain.Types.Log
import Nuchain.Types.Dispatch (Dispatch)
import qualified Nuchain.Types.Dispatch as D
import qualified Nuchain.Types.History as History
import qualified Nuchain.Log as Log
import Nuchain.Types.Comms (Comms(..))
import Nuchain.Types.Metric
import Nuchain.Command
import Nuchain.Event (pprintBeat)
import Nuchain.Private.Service (decrypt)
import Nuchain.Types.Crypto
import Nuchain.Types.Private (PrivatePlaintext(..),PrivateResult(..))
import Nuchain.Execution.Pact
import Nuchain.Consensus.Publish
import Nuchain.Types.Entity
import qualified Nuchain.ConfigChange as CfgChange

initExecutionEnv
  :: Dispatch
  -> (String -> IO ())
  -> PactPersistConfig
  -> LogRules
  -> (Metric -> IO ())
  -> IO UTCTime
  -> GlobalConfigTMVar
  -> EntityConfig
  -> ExecutionEnv
initExecutionEnv dispatch' debugPrint' persistConfig logRules'
                 publishMetric' getTimestamp' gcm' ent = ExecutionEnv
  { _eenvExecChannel = dispatch' ^. D.dispExecService
  , _eenvHistoryChannel = dispatch' ^. D.dispHistoryChannel
  , _eenvPrivateChannel = dispatch' ^. D.dispPrivateChannel
  , _eenvPactPersistConfig = persistConfig
  , _eenvDebugPrint = debugPrint'
  , _eenvExecLoggers = initLoggers debugPrint' doLog logRules'
  , _eenvPublishMetric = publishMetric'
  , _eenvGetTimestamp = getTimestamp'
  , _eenvMConfig = gcm'
  , _eenvEntityConfig = ent
  }

data ReplayStatus = ReplayFromDisk | FreshCommands deriving (Show, Eq)

onUpdateConf :: ExecutionChannel -> Config -> IO ()
onUpdateConf oChan conf@Config{ _nodeId = nodeId' } = do
  writeComm oChan $ ChangeNodeId nodeId'
  writeComm oChan $ UpdateKeySet $ confToKeySet conf


runExecutionService :: ExecutionEnv -> Publish -> NodeId -> KeySet -> IO ()
runExecutionService env pub nodeId' keySet' = do
  cmdExecInter <- initPactService env pub
  initExecutionState <- return ExecutionState {
    _esNodeId = nodeId',
    _esKeySet = keySet',
    _esKCommandExecInterface = cmdExecInter,
    _esModuleCache = mempty}
  let cu = ConfigUpdater (_eenvDebugPrint env) "Service|Execution" (onUpdateConf (_eenvExecChannel env))
  linkAsyncTrack "ExecutionConfUpdater" $ CfgChange.runConfigUpdater cu (env ^. eenvMConfig)
  void $ runRWST handle env initExecutionState

debug :: String -> ExecutionService ()
debug s = do
  unless (null s) $ do
    dbg <- view eenvDebugPrint
    liftIO $! dbg $ "[Service|Execution] " ++ s

now :: ExecutionService UTCTime
now = view eenvGetTimestamp >>= liftIO

logMetric :: Metric -> ExecutionService ()
logMetric m = do
  publishMetric' <- view eenvPublishMetric
  liftIO $! publishMetric' m

handle :: ExecutionService ()
handle = do
  oChan <- view eenvExecChannel
  debug "Launch!"
  forever $ do
    q <- liftIO $ readComm oChan
    case q of
      ExecutionBeat t -> do
        gCfg <- view eenvMConfig
        conf <- liftIO $ Cfg.readCurrentConfig gCfg
        liftIO (pprintBeat t conf) >>= debug
      ChangeNodeId{..} -> do
        prevNodeId <- use esNodeId
        unless (prevNodeId == newNodeId) $ do
          esNodeId .= newNodeId
          debug $ "Changed NodeId: " ++ show prevNodeId ++ " -> " ++ show newNodeId
      UpdateKeySet{..} -> do
        prevKeySet <- use esKeySet
        unless (prevKeySet == newKeySet) $ do
          esKeySet .= newKeySet
          debug $ "Updated keyset"
      ExecuteNewEntries{..} -> do
        debug $ (show . Log.lesCnt $ logEntriesToApply)
              ++ " new log entries to apply, up to "
              ++ show (fromJust $ Log.lesMaxIndex logEntriesToApply)
        applyLogEntries FreshCommands logEntriesToApply
      ReloadFromDisk{..} -> do
        debug $ (show . Log.lesCnt $ logEntriesToApply)
              ++ " entries loaded from disk to apply, up to "
              ++ show (fromJust $ Log.lesMaxIndex logEntriesToApply)
        applyLogEntries ReplayFromDisk logEntriesToApply
      ExecLocal{..} -> applyLocalCommand (localCmd,localResult)
      ExecConfigChange{..} -> applyConfigChange logEntriesToApply

applyLogEntries :: ReplayStatus -> LogEntries -> ExecutionService ()
applyLogEntries rs les@(LogEntries leToApply) = do
  now' <- now
  (results :: [(RequestKey, CommandResult)]) <- mapM (applyCommand now') (Map.elems leToApply)
  commitIndex' <- return $ fromJust $ Log.lesMaxIndex les
  logMetric $ MetricAppliedIndex commitIndex'
  if not (null results)
    then do
      debug $! "Applied " ++ show (length results) ++ " CMD(s)"
      hChan <- view eenvHistoryChannel
      unless (rs == ReplayFromDisk) $ liftIO $! writeComm hChan (History.Update $ HashMap.fromList results)
    else debug "Applied log entries but did not send results?"

logApplyLatency :: UTCTime -> LogEntry -> ExecutionService ()
logApplyLatency startTime LogEntry{..} = case _leCmdLatMetrics of
  Nothing -> return ()
  Just n ->
    logMetric $ MetricApplyLatency $ fromIntegral $ interval (_lmFirstSeen n) startTime
{-# INLINE logApplyLatency #-}

getPendingPreProcSCC :: UTCTime -> MVar SCCPreProcResult -> ExecutionService SCCPreProcResult
getPendingPreProcSCC startTime mvResult = liftIO (tryReadMVar mvResult) >>= \case
  Just r -> return r
  Nothing -> do
    r <- liftIO $ readMVar mvResult
    endTime <- now
    debug $ "Blocked on Pending Pact PreProc for " ++ printInterval startTime endTime
    return r

getPendingPreProcCCC :: UTCTime -> MVar CCCPreProcResult -> ExecutionService CCCPreProcResult
getPendingPreProcCCC startTime mvResult = liftIO (tryReadMVar mvResult) >>= \case
  Just r -> return r
  Nothing -> do
    r <- liftIO $ readMVar mvResult
    endTime <- now
    debug $ "Blocked on Consensus PreProc for " ++ printInterval startTime endTime
    return r

applyCommand :: UTCTime -> LogEntry -> ExecutionService (RequestKey, CommandResult)
applyCommand _tEnd le@LogEntry{..} = do
  apply <- _kceiApplyPPCmd <$> use esKCommandExecInterface
  moduleCache <- use esModuleCache
  startTime <- now
  logApplyLatency startTime le
  let chash = Pact.toUntypedHash $ getCmdBodyHash _leCommand
      rkey = RequestKey chash
      stamp ppLat = do
        tEnd' <- now
        return $ mkLatResults <$> updateExecutionPreProc startTime tEnd' ppLat
  case _leCommand of
    SmartContractCommand{..} -> do
      (pproc, ppLat) <- case _sccPreProc of
        Unprocessed -> do
          debug $ "WARNING: non-preproccessed command found for " ++ show _leLogIndex
          case (verifyCommand _leCommand) of
            SmartContractCommand{..} -> return $! (result _sccPreProc, _leCmdLatMetrics)
            _ -> error "[applyCommand.1] unreachable exception... and yet reached"
        Pending{..} -> do
          PendingResult{..} <- getPendingPreProcSCC startTime pending
          return $ (_prResult, updateLatPreProc _prStartedPreProc _prFinishedPreProc _leCmdLatMetrics)
        Result{..}  -> do
          debug $ "WARNING: fully resolved pact command found for " ++ show _leLogIndex
          return $! (result, _leCmdLatMetrics)
      (T2 result moduleCache') <- liftIO $ apply Pact.Transactional moduleCache _sccCmd pproc
      lm <- stamp ppLat
      assign esModuleCache moduleCache' -- update the cache stored in ExecutionState
      return ( rkey
             , SmartContractResult
               { _scrPactResult = PactContractResult
                 { _pcrHash = chash
                 , _pcrResult = result
                 , _pcrLogIndex = _leLogIndex
                 , _pcrLatMetrics = lm
                 }
               }
             )
    ConsensusChangeCommand{..} -> do
      (pproc, ppLat) <- case _cccPreProc of
        Unprocessed -> do
          debug $ "WARNING: non-preproccessed config command found for " ++ show _leLogIndex
          case (verifyCommand _leCommand) of
            ConsensusChangeCommand{..} -> return $! (result _cccPreProc, _leCmdLatMetrics)
            _ -> error "[applyCommand.conf.2] unreachable exception... and yet reached"
        Pending{..} -> do
          PendingResult{..} <- getPendingPreProcCCC startTime pending
          return $ (_prResult, updateLatPreProc _prStartedPreProc _prFinishedPreProc _leCmdLatMetrics)
        Result{..}  -> do
          debug $ "WARNING: fully resolved consensus command found for " ++ show _leLogIndex
          return $! (result, _leCmdLatMetrics)
      gcm <- view eenvMConfig
      result <- liftIO $ CfgChange.mutateGlobalConfig gcm pproc
      lm <- stamp ppLat
      -- Note: no module cache update required for ConcensusChangeCommand
      return ( rkey
             , ConsensusChangeResult
               { _crHash = chash
               , _concrResult = result
               , _crLogIndex = _leLogIndex
               , _crLatMetrics = lm
               })
    PrivateCommand Hashed{..} -> do
      pchan <- view eenvPrivateChannel
      r <- liftIO $ decrypt pchan _hValue
      let finish pr = stamp _leCmdLatMetrics >>= \l ->
            return (rkey, PrivateCommandResult chash pr _leLogIndex l)
      case r of
        -- Note: module cache updates for PrivateCommands are done in applyPrivate
        Left e -> finish (PrivateFailure (show e))
        Right Nothing -> finish PrivatePrivate
        Right (Just pm) -> do
          pr <- applyPrivate le pm
          case pr of
            Left e -> finish $ PrivateFailure e
            Right cr -> finish $ PrivateSuccess cr

applyPrivate :: LogEntry -> PrivatePlaintext -> ExecutionService (Either String (Pact.CommandResult Hash))
applyPrivate LogEntry{..} PrivatePlaintext{..} = case decode _ppMessage of
  Left e -> return $ Left e
  Right cmd -> case Pact.verifyCommand cmd of
    Pact.ProcFail e -> return $ Left e
    p@Pact.ProcSucc {} -> do
      moduleCache <- use esModuleCache
      apply <- _kceiApplyPPCmd <$> use esKCommandExecInterface
      (T2 result moduleCache') <- liftIO (apply Pact.Transactional moduleCache cmd p)
      assign esModuleCache moduleCache' -- update the cache stored in ExecutionState
      liftIO $ return $ Right result


applyLocalCommand
  :: (Pact.Command ByteString, MVar (Pact.CommandResult Pact.Hash))
  -> ExecutionService ()
applyLocalCommand (cmd, mv) = do
  moduleCache <- use esModuleCache
  applyLocal <- _kceiApplyCmd <$> use esKCommandExecInterface
  (T2 cr moduleCache') <- liftIO $ applyLocal Pact.Local moduleCache cmd
  assign esModuleCache moduleCache' -- update the cache stored in ExecutionState
  liftIO $ putMVar mv cr

-- | This may be used in the future for configuration changes other than cluster membership changes.
--   Cluster membership changes are implemented via applyCommand
applyConfigChange :: LogEntries -> ExecutionService ()
applyConfigChange _ = debug "[Execution service]: applyConfigChange - not implemented"

updateLatPreProc :: Maybe UTCTime -> Maybe UTCTime -> Maybe CmdLatencyMetrics -> Maybe CmdLatencyMetrics
updateLatPreProc hitPreProc finPreProc = fmap update'
  where
    update' cmd = cmd {_lmHitPreProc = hitPreProc
                      ,_lmFinPreProc = finPreProc}
{-# INLINE updateLatPreProc #-}

updateExecutionPreProc :: UTCTime -> UTCTime -> Maybe CmdLatencyMetrics -> Maybe CmdLatencyMetrics
updateExecutionPreProc hitExecution finExecution = fmap update'
  where
    update' cmd = cmd {_lmHitExecution = Just hitExecution
                      ,_lmFinExecution = Just finExecution}
{-# INLINE updateExecutionPreProc #-}
