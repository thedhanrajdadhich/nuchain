{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Nuchain.Types.Execution
  ( ApplyFn
  , Execution(..)
  , ExecutionEnv(..)
  , eenvExecChannel, eenvDebugPrint, eenvPublishMetric
  , eenvGetTimestamp, eenvHistoryChannel, eenvMConfig
  , eenvPactPersistConfig, eenvExecLoggers, eenvEntityConfig
  , eenvPrivateChannel
  , ExecutionState(..)
  , KCommandExecInterface(..)
  , esNodeId, esKeySet, esKCommandExecInterface, esModuleCache
  , ExecutionChannel(..)
  , ExecutionService
  , ModuleCache
  ) where

import Control.Lens hiding (Index)

import Control.Monad.Trans.RWS.Strict (RWST)
import Control.Concurrent.Chan (Chan)
import Control.Concurrent (MVar)

import Data.HashMap.Strict (HashMap)
import Data.Thyme.Clock (UTCTime)
import Data.Tuple.Strict (T2(..))
import Data.ByteString (ByteString)

import qualified Pact.Types.ChainMeta as Pact
import qualified Pact.Types.Hash as Pact (Hash)
import qualified Pact.Types.Command as Pact
import Pact.Types.Logger (Loggers)
import qualified Pact.Types.Term as Pact (ModuleName, Ref)
import qualified Pact.Types.Persistence as Pact (ExecutionMode, ModuleData)

import Nuchain.Types.Base (NodeId)
import Nuchain.Types.PactDB
import Nuchain.Types.Config (GlobalConfigTMVar)
import Nuchain.Types.Comms (Comms(..),initCommsNormal,readCommNormal,writeCommNormal)
import Nuchain.Types.Crypto
import Nuchain.Types.Metric (Metric)
import Nuchain.Types.Log (LogEntry,LogEntries)
import Nuchain.Types.Event (Beat)
import Nuchain.Types.History (HistoryChannel)
import Nuchain.Types.Private (PrivateChannel)
import Nuchain.Types.Entity (EntityConfig)

type ModuleCache = HashMap Pact.ModuleName (Pact.ModuleData Pact.Ref, Bool)

type ApplyFn = LogEntry -> IO (Pact.CommandResult (Pact.Hash))

data Execution =
  ReloadFromDisk { logEntriesToApply :: !LogEntries } |
  ExecuteNewEntries { logEntriesToApply :: !LogEntries } |
  ChangeNodeId { newNodeId :: !NodeId } |
  UpdateKeySet { newKeySet :: !KeySet } |
  ExecutionBeat Beat |
  ExecLocal { localCmd :: !(Pact.Command ByteString),
              localResult :: !(MVar (Pact.CommandResult Pact.Hash)) } |
  ExecConfigChange { logEntriesToApply :: !LogEntries }

newtype ExecutionChannel = ExecutionChannel (Chan Execution)

instance Comms Execution ExecutionChannel where
  initComms = ExecutionChannel <$> initCommsNormal
  readComm (ExecutionChannel c) = readCommNormal c
  writeComm (ExecutionChannel c) = writeCommNormal c

data ExecutionEnv = ExecutionEnv
  { _eenvExecChannel :: !ExecutionChannel
  , _eenvHistoryChannel :: !HistoryChannel
  , _eenvPrivateChannel :: !PrivateChannel
  , _eenvPactPersistConfig :: !PactPersistConfig
  , _eenvDebugPrint :: !(String -> IO ())
  , _eenvExecLoggers :: !Loggers
  , _eenvPublishMetric :: !(Metric -> IO ())
  , _eenvGetTimestamp :: !(IO UTCTime)
  , _eenvMConfig :: GlobalConfigTMVar
  , _eenvEntityConfig :: !EntityConfig
  }
makeLenses ''ExecutionEnv

type KApplyCmd l = Pact.ExecutionMode -> ModuleCache -> Pact.Command ByteString
                 -> IO (T2 (Pact.CommandResult l) ModuleCache)
type KApplyPPCmd m a l = Pact.ExecutionMode -> ModuleCache -> Pact.Command ByteString
                       -> Pact.ProcessedCommand m a -> IO (T2 (Pact.CommandResult l) ModuleCache)

data KCommandExecInterface m a l = KCommandExecInterface
  { _kceiApplyCmd :: KApplyCmd l
  , _kceiApplyPPCmd :: KApplyPPCmd m a l
  }

data ExecutionState = ExecutionState
  { _esNodeId :: !NodeId
  , _esKeySet :: !KeySet
  , _esKCommandExecInterface :: !(KCommandExecInterface Pact.PrivateMeta Pact.ParsedCode Pact.Hash)
  , _esModuleCache :: ModuleCache
  }
makeLenses ''ExecutionState

type ExecutionService = RWST ExecutionEnv () ExecutionState IO
