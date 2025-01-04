module Nuchain.Consensus.Handle
  ( handleEvents )
where

import Control.Concurrent (tryTakeMVar)
import Control.Lens hiding ((:>))
import Control.Monad
import Control.Monad.IO.Class

import Nuchain.Config.TMVar as Cfg
import Nuchain.Event
import Nuchain.Types
import Nuchain.Consensus.Util

import qualified Nuchain.Consensus.Handle.AppendEntries as PureAppendEntries
import qualified Nuchain.Consensus.Handle.Command as PureCommand
import qualified Nuchain.Consensus.Handle.ElectionTimeout as PureElectionTimeout
import qualified Nuchain.Consensus.Handle.HeartbeatTimeout as PureHeartbeatTimeout
import qualified Nuchain.Consensus.Handle.RequestVote as PureRequestVote
import qualified Nuchain.Consensus.Handle.RequestVoteResponse as PureRequestVoteResponse

handleEvents :: Consensus ()
handleEvents = forever $ do
  timerTarget' <- use csTimerTarget
  -- we use the MVar to preempt a backlog of messages when under load. This happens during a large 'many test'
  tFired <- liftIO $ tryTakeMVar timerTarget'
  logStaticMetrics
  e <- case tFired of
    Nothing -> dequeueEvent
    Just v -> return v
  case e of
    ERPC rpc                      -> handleRPC rpc
    NewCmd cmds                   -> PureCommand.handleBatch cmds
    ElectionTimeout s             -> do
      debug "*%* Election timeout received from timer thread"
      PureElectionTimeout.handle s
    HeartbeatTimeout s            -> PureHeartbeatTimeout.handle s
    Heart tock'  -> do
      gCfg <- view cfg
      conf <- liftIO $ Cfg.readCurrentConfig gCfg
      liftIO (pprintBeat tock' conf) >>= debug

-- TODO: prune out AER's from RPC if possible
handleRPC :: RPC -> Consensus ()
handleRPC rpc = case rpc of
  AE' ae          -> PureAppendEntries.handle ae
  AER' aer        -> error $ "Invariant Error: AER received by Consensus Service" ++ show aer
  RV' rv          -> PureRequestVote.handle rv
  RVR' rvr        -> PureRequestVoteResponse.handle rvr
  NEW' _          -> error "Invariant Error: new commands should never be `RPC (NEW' _) :: Event`, use `NewCmd :: Event` instead"
