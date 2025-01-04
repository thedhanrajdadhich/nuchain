{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Nuchain.Types.Sender
  ( SenderServiceChannel(..)
  , ServiceRequest'(..)
  , ServiceRequest(..)
  , StateSnapshot(..), snapNodeId, snapNodeRole, snapClusterMembers, snapLeader, snapTerm
  , snapPublicKey
  , snapPrivateKey, snapYesVotes
  , AEBroadcastControl(..)
  ) where

import Control.Lens
import Control.Concurrent.Chan (Chan)

import qualified Crypto.Ed25519.Pure as Ed25519

import Data.Set (Set)
import Data.Thyme.Clock

import Nuchain.Types.Base
import Nuchain.Types.Message
import Nuchain.Types.Comms
import Nuchain.Config.ClusterMembership
import Nuchain.Types.Event (Beat)

data ServiceRequest' =
  ServiceRequest'
  { _unSS :: StateSnapshot
  , _unSR :: ServiceRequest } |
  ForwardCommandToLeader
  { _srCommands :: !NewCmdRPC} |
  SendAllAppendEntriesResponse
  { _srLastLogHash :: !LogIndex
  , _srMaxIndex :: !Hash
  , _srIssuedTime :: !UTCTime} |
  SenderBeat Beat
  deriving (Eq, Show)

newtype SenderServiceChannel = SenderServiceChannel (Chan ServiceRequest')

instance Comms ServiceRequest' SenderServiceChannel where
  initComms = SenderServiceChannel <$> initCommsNormal
  readComm (SenderServiceChannel c) = readCommNormal c
  writeComm (SenderServiceChannel c) = writeCommNormal c

data AEBroadcastControl =
  SendAERegardless -- ^ Causes an AE to be sent no matter what. Use with care, under load we don't want to send > 1 replication AE per heartbeat period
  | OnlySendIfFollowersAreInSync -- ^ Only dispatches the AE if follower are all in sync (steady state) AND if the last AE to replicate entries was > 1 HB Timeout ago
  deriving (Show, Eq)

data ServiceRequest =
    BroadcastAE
    { _srAeBoardcastControl :: !AEBroadcastControl } |
    EstablishDominance |
    SingleAER
    { _srFor :: !NodeId
    , _srSuccess :: !Bool
    , _srConvinced :: !Bool }|
    -- TODO: we can be smarter here and fill in the details the AER needs about the logs without needing to hit that thread
    BroadcastAER | -- TODO: verify that we did ^ and this can be removed
    BroadcastRV RequestVote|
    BroadcastRVR
    { _srCandidate :: !NodeId
    , _srHeardFromLeader :: !(Maybe HeardFromLeader)
    , _srVote :: !Bool}
    deriving (Eq, Show)

data StateSnapshot = StateSnapshot
  { _snapNodeId :: !NodeId
  , _snapNodeRole :: !Role
  , _snapClusterMembers :: !ClusterMembership
  , _snapLeader :: !(Maybe NodeId)
  , _snapTerm :: !Term
  , _snapPublicKey :: !Ed25519.PublicKey
  , _snapPrivateKey :: !Ed25519.PrivateKey
  , _snapYesVotes :: !(Set RequestVoteResponse)
  } deriving (Eq, Show)
makeLenses ''StateSnapshot
