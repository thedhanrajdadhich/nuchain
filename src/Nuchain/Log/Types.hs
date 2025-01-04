{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Nuchain.Log.Types
  ( LogState(..)
  , lsVolatileLogEntries, lsPersistedLogEntries, lsLastApplied, lsLastLogIndex, lsNextLogIndex, lsCommitIndex
  , lsLastPersisted, lsLastLogTerm, lsLastLogHash, lsLastInMemory
  , initLogState, lesEmpty, plesEmpty
  , LogEnv(..)
  , logQueryChannel, execChannel, consensusEvent, senderChannel, debugPrint
  , dbConn, evidence, publishMetric
  , persistedLogEntriesToKeepInMemory
  , config
  , LogThread
  , LogServiceChannel(..)
  , UpdateLogs(..)
  , QueryApi(..)
  -- ReExports
  , LogIndex(..)
  , KeySet(..)
  -- for tesing
  ) where

import Control.Lens hiding (Index, (|>))

import Control.Concurrent (MVar)
import Control.Concurrent.Chan (Chan)
import Control.Monad.Trans.RWS.Strict

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)

import Database.SQLite.Simple (Connection(..))

import GHC.Generics

import qualified Pact.Types.Hash as P

import qualified Nuchain.Config.TMVar as Cfg
import Nuchain.Types.Crypto
import Nuchain.Types.Base as X
import Nuchain.Types.Log
import Nuchain.Types.Comms
import Nuchain.Types.Event (Beat,ConsensusEventChannel)
import Nuchain.Types.Evidence (EvidenceChannel)
import Nuchain.Types.Execution (ExecutionChannel)
import Nuchain.Types.Metric
import Nuchain.Types.Sender (SenderServiceChannel)

data QueryApi =
  Query (Set AtomicQuery) (MVar (Map AtomicQuery QueryResult)) |
  Update UpdateLogs |
  NeedCacheEvidence (Set LogIndex) (MVar (Map LogIndex Hash)) |
  Heart Beat
  deriving (Eq)

newtype LogServiceChannel = LogServiceChannel (Chan QueryApi)

instance Comms QueryApi LogServiceChannel where
  initComms = LogServiceChannel <$> initCommsNormal
  readComm (LogServiceChannel c) = readCommNormal c
  writeComm (LogServiceChannel c) = writeCommNormal c

data LogEnv = LogEnv
  { _logQueryChannel :: !LogServiceChannel
  , _consensusEvent :: !ConsensusEventChannel
  , _execChannel :: !ExecutionChannel
  , _evidence :: !EvidenceChannel
  , _senderChannel :: !SenderServiceChannel
  , _persistedLogEntriesToKeepInMemory :: !Int
  , _debugPrint :: !(String -> IO ())
  , _dbConn :: !(Maybe Connection)
  , _publishMetric :: !(Metric -> IO ())
  , _config :: !Cfg.GlobalConfigTMVar}
makeLenses ''LogEnv

data LogState = LogState
  { _lsVolatileLogEntries  :: !LogEntries
  , _lsPersistedLogEntries :: !PersistedLogEntries
  , _lsLastApplied      :: !LogIndex
  , _lsLastLogIndex     :: !LogIndex
  , _lsLastLogHash      :: !Hash
  , _lsNextLogIndex     :: !LogIndex
  , _lsCommitIndex      :: !LogIndex
  , _lsLastPersisted    :: !LogIndex
  , _lsLastInMemory     :: !(Maybe LogIndex)
  , _lsLastLogTerm      :: !Term
  } deriving (Show, Eq, Generic)
makeLenses ''LogState

initLogState :: LogState
initLogState = LogState
  { _lsVolatileLogEntries = lesEmpty
  , _lsPersistedLogEntries = plesEmpty
  , _lsLastApplied = startIndex
  , _lsLastLogIndex = startIndex
  , _lsLastLogHash = P.pactInitialHash
  , _lsNextLogIndex = startIndex + 1
  , _lsCommitIndex = startIndex
  , _lsLastPersisted = startIndex
  , _lsLastInMemory = Nothing
  , _lsLastLogTerm = startTerm
  }

lesEmpty :: LogEntries
lesEmpty = LogEntries Map.empty
{-# INLINE lesEmpty #-}

plesEmpty :: PersistedLogEntries
plesEmpty = PersistedLogEntries Map.empty
{-# INLINE plesEmpty #-}

type LogThread = RWST LogEnv () LogState IO
