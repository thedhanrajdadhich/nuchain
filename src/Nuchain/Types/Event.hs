{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Nuchain.Types.Event
  ( Event(..)
  , Beat(..)
  , ResetLeaderNoFollowersTimeout(..)
  , ConsensusEvent(..)
  , ConsensusEventChannel(..)
  ) where

import Control.Concurrent.BoundedChan (BoundedChan)

import Data.Typeable

import Data.Thyme.Clock (UTCTime)

import Nuchain.Types.Message

import Nuchain.Types.Command
import Nuchain.Types.Comms

data Beat = Beat
  { _tockTargetDelay :: !Int
  , _tockStartTime :: !UTCTime
  } deriving (Show, Eq)

data ResetLeaderNoFollowersTimeout = ResetLeaderNoFollowersTimeout
  deriving Show

data Event = ERPC RPC
           | NewCmd ![(Maybe CmdLatencyMetrics, Command)]
           | ElectionTimeout String
           | HeartbeatTimeout String
           | Heart Beat
  deriving (Show)

newtype ConsensusEvent = ConsensusEvent { _unConsensusEvent :: Event}
  deriving (Show, Typeable)

newtype ConsensusEventChannel = ConsensusEventChannel (BoundedChan ConsensusEvent)

instance Comms ConsensusEvent ConsensusEventChannel where
  initComms = ConsensusEventChannel <$> initCommsBounded
  readComm (ConsensusEventChannel c) = readCommBounded c
  writeComm (ConsensusEventChannel c) = writeCommBounded c
  {-# INLINE initComms #-}
  {-# INLINE readComm #-}
  {-# INLINE writeComm #-}
