{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Nuchain.Types.Spec
  ( Consensus
  , ConsensusSpec(..)
  , debugPrint, publishMetric, getTimestamp, random
  , viewConfig, readConfig, csTimerTarget, evidenceState, timeCache
  , ConsensusEnv(..), cfg, enqueueLogQuery, rs
  , enqueue, enqueueMultiple, dequeue, enqueueLater, killEnqueued
  , sendMessage, clientSendMsg, mResetLeaderNoFollowers, mPubConsensus
  , informEvidenceServiceOfElection, enqueueHistoryQuery
  , ConsensusState(..), initialConsensusState
  , csNodeRole, csTerm, csVotedFor, csLazyVote, csCurrentLeader, csIgnoreLeader
  , csTimerKey, csYesVotes, csPotentialVotes, csLastCommitTime
  , csTimeSinceLastAER, csCmdBloomFilter, csInvalidCandidateResults
  , mkConsensusEnv
  , PublishedConsensus(..),pcLeader,pcRole,pcTerm,pcYesVotes
  , LazyVote(..), lvVoteFor, lvAllReceived
  , InvalidCandidateResults(..), icrMyReqVoteSig, icrNoVotes
  ) where

import Control.Concurrent (MVar, yield, tryPutMVar, tryTakeMVar)
import Control.Concurrent.STM
import Control.Exception (mask_)
import Control.Lens hiding (Index, (|>))
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.RWS.Strict (RWST)

import qualified Crypto.Ed25519.Pure as Ed25519

import Data.BloomFilter (Bloom)
import qualified Data.BloomFilter as Bloom
import qualified Data.BloomFilter.Hash as BHash
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Thyme.Clock
import Data.Thyme.Time.Core ()
import GHC.Event (TimeoutKey, getSystemTimerManager, registerTimeout, unregisterTimeout)
import System.Random (Random)

import Nuchain.Types.Base
import Nuchain.Config.TMVar
import Nuchain.Types.Event
import Nuchain.Types.Message
import Nuchain.Types.Metric
import Nuchain.Types.Comms
import Nuchain.Types.Dispatch
import Nuchain.Types.Sender (SenderServiceChannel, ServiceRequest')
import Nuchain.Log.Types (QueryApi(..))
import Nuchain.Types.History (History(..))
import Nuchain.Types.Evidence (PublishedEvidenceState, Evidence(ClearConvincedNodes))

data PublishedConsensus = PublishedConsensus
    { _pcLeader :: !(Maybe NodeId)
    , _pcRole :: !Role
    , _pcTerm :: !Term
    , _pcYesVotes :: !(Set RequestVoteResponse)
    }
makeLenses ''PublishedConsensus

data ConsensusSpec = ConsensusSpec
  {
    -- | Function to log a debug message (no newline).
    _debugPrint       :: !(String -> IO ())
  , _publishMetric    :: !(Metric -> IO ())
  , _getTimestamp     :: !(IO UTCTime)
  , _random           :: !(forall a . Random a => (a, a) -> IO a)
  }
makeLenses (''ConsensusSpec)

data InvalidCandidateResults = InvalidCandidateResults
  { _icrMyReqVoteSig :: !Ed25519.Signature
  , _icrNoVotes :: !(Set NodeId)
  } deriving (Show, Eq)
makeLenses ''InvalidCandidateResults

data LazyVote = LazyVote
  { _lvVoteFor :: !RequestVote
  , _lvAllReceived :: !(Map NodeId RequestVote)
  } deriving (Show, Eq)
makeLenses ''LazyVote

data ConsensusState = ConsensusState
  { _csNodeRole         :: !Role
  , _csTerm             :: !Term
  , _csVotedFor         :: !(Maybe NodeId)
  , _csLazyVote         :: !(Maybe LazyVote)
  , _csCurrentLeader    :: !(Maybe NodeId)
  , _csIgnoreLeader     :: !Bool
  , _csTimerKey         :: !(Maybe TimeoutKey)
  , _csTimerTarget      :: !(MVar Event)
  , _csCmdBloomFilter   :: !(Bloom RequestKey)
  , _csYesVotes        :: !(Set RequestVoteResponse)
  , _csPotentialVotes  :: !(Set NodeId)
  , _csTimeSinceLastAER :: !Int
  -- used for metrics
  , _csLastCommitTime   :: !(Maybe UTCTime)
  -- used only during Candidate State
  , _csInvalidCandidateResults :: !(Maybe InvalidCandidateResults)
  }
makeLenses ''ConsensusState

initialConsensusState :: MVar Event -> ConsensusState
initialConsensusState timerTarget' = ConsensusState
{-role-}                Follower
{-term-}                startTerm
{-votedFor-}            Nothing
{-lazyVote-}            Nothing
{-currentLeader-}       Nothing
{-ignoreLeader-}        False
{-timerThread-}         Nothing
{-timerTarget-}         timerTarget'
{-cmdBloomFilter-}      (Bloom.empty (\(RequestKey (Hash k)) -> BHash.cheapHashes 30 k) 134217728)
{-cYesVotes-}           Set.empty
{-cPotentialVotes-}     Set.empty
{-timeSinceLastAER-}    0
{-lastCommitTime-}      Nothing
{-invalidCandidateResults-} Nothing

type Consensus = RWST ConsensusEnv () ConsensusState IO

data ConsensusEnv = ConsensusEnv
  { _cfg              :: !(GlobalConfigTMVar)
  , _enqueueLogQuery  :: !(QueryApi -> IO ())
  , _enqueueHistoryQuery :: !(History -> IO ())
  , _rs               :: !ConsensusSpec
  , _sendMessage      :: !(ServiceRequest' -> IO ())
  , _enqueue          :: !(Event -> IO ())
  , _enqueueMultiple  :: !([Event] -> IO ())
  , _enqueueLater     :: !(Int -> Event -> IO TimeoutKey)
  , _killEnqueued     :: !(TimeoutKey -> IO ())
  , _dequeue          :: !(IO Event)
  , _clientSendMsg    :: !(OutboundGeneral -> IO ())
  , _evidenceState    :: !(MVar PublishedEvidenceState)
  , _timeCache        :: !(IO UTCTime)
  , _mResetLeaderNoFollowers :: !(MVar ResetLeaderNoFollowersTimeout)
  , _informEvidenceServiceOfElection :: !(IO ())
  , _mPubConsensus    :: !(MVar PublishedConsensus)
  }
makeLenses ''ConsensusEnv

mkConsensusEnv
  :: GlobalConfigTMVar
  -> ConsensusSpec
  -> Dispatch
  -> MVar Event
  -> IO UTCTime
  -> MVar PublishedEvidenceState
  -> MVar ResetLeaderNoFollowersTimeout
  -> MVar PublishedConsensus
  -> ConsensusEnv
mkConsensusEnv conf' rSpec dispatch timerTarget' timeCache' mEs mResetLeaderNoFollowers' mPubConsensus' = ConsensusEnv
    { _cfg = conf'
    , _enqueueLogQuery = writeComm ls'
    , _enqueueHistoryQuery = writeComm hs'
    , _rs = rSpec
    , _sendMessage = sendMsg g'
    , _enqueue = writeComm ie' . ConsensusEvent
    , _enqueueMultiple = mapM_ (writeComm ie' . ConsensusEvent)
    , _enqueueLater = timerFn timerTarget'
    , _killEnqueued = cancelTimerFn
    , _dequeue = _unConsensusEvent <$> readComm ie'
    , _clientSendMsg = writeComm cog'
    , _evidenceState = mEs
    , _timeCache = timeCache'
    , _mResetLeaderNoFollowers = mResetLeaderNoFollowers'
    , _informEvidenceServiceOfElection = writeComm ev' ClearConvincedNodes
    , _mPubConsensus = mPubConsensus'
    }
  where
    g' = dispatch ^. dispSenderService
    cog' = dispatch ^. dispOutboundGeneral
    ls' = dispatch ^. dispLogService
    hs' = dispatch ^. dispHistoryChannel
    ie' = dispatch ^. dispConsensusEvent
    ev' = dispatch ^. dispEvidence

timerFn :: MVar Event -> Int -> Event -> IO TimeoutKey
timerFn timerMVar t event = mask_ $ do
  void $ tryTakeMVar timerMVar
  mgr <- getSystemTimerManager
  registerTimeout mgr t $ do
    b <- tryPutMVar timerMVar $! event
    unless b (putStrLn "Failed to update timer MVar")

cancelTimerFn :: TimeoutKey -> IO ()
cancelTimerFn k = mask_ $ do
  mgr <- getSystemTimerManager
  unregisterTimeout mgr k

sendMsg :: SenderServiceChannel -> ServiceRequest' -> IO ()
sendMsg outboxWrite og = do
  writeComm outboxWrite og
  yield

readConfig :: Consensus Config
readConfig = view cfg >>= fmap _gcConfig . liftIO . atomically . readTMVar

viewConfig :: Getting r Config r -> Consensus r
viewConfig l = do
  (c :: Config) <- view cfg >>= fmap _gcConfig . liftIO . atomically . readTMVar
  return $ view l c
