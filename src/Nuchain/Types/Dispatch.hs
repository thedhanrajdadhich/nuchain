{-# LANGUAGE TemplateHaskell #-}

module Nuchain.Types.Dispatch
  ( Dispatch(..)
  , initDispatch
  , dispInboundAER
  , dispInboundCMD
  , dispInboundRVorRVR
  , dispInboundGeneral
  , dispOutboundGeneral
  , dispConsensusEvent
  , dispSenderService
  , dispLogService
  , dispEvidence
  , dispExecService
  , dispHistoryChannel
  , dispProcessRequestChannel
  , dispPrivateChannel
  ) where

import Control.Lens

import Data.Typeable

import Nuchain.Types.Comms
import Nuchain.Types.Sender (SenderServiceChannel)
import Nuchain.Log.Types (LogServiceChannel)
import Nuchain.Evidence.Spec (EvidenceChannel)
import Nuchain.Types.Execution (ExecutionChannel)
import Nuchain.Types.History (HistoryChannel)
import Nuchain.Types.PreProc (ProcessRequestChannel)
import Nuchain.Types.Private (PrivateChannel)
import Nuchain.Types.Message (InboundCMDChannel,OutboundGeneralChannel)
import Nuchain.Types.Event (ConsensusEventChannel)

data Dispatch = Dispatch
  { _dispInboundAER      :: InboundAERChannel
  , _dispInboundCMD      :: InboundCMDChannel
  , _dispInboundRVorRVR  :: InboundRVorRVRChannel
  , _dispInboundGeneral  :: InboundGeneralChannel
  , _dispOutboundGeneral :: OutboundGeneralChannel
  , _dispConsensusEvent   :: ConsensusEventChannel
  , _dispSenderService   :: SenderServiceChannel
  , _dispLogService   :: LogServiceChannel
  , _dispEvidence   :: EvidenceChannel
  , _dispExecService :: ExecutionChannel
  , _dispHistoryChannel :: HistoryChannel
  , _dispProcessRequestChannel :: ProcessRequestChannel
  , _dispPrivateChannel :: PrivateChannel
  } deriving (Typeable)

initDispatch :: IO Dispatch
initDispatch = Dispatch
  <$> initComms
  <*> initComms
  <*> initComms
  <*> initComms
  <*> initComms
  <*> initComms
  <*> initComms
  <*> initComms
  <*> initComms
  <*> initComms
  <*> initComms
  <*> initComms
  <*> initComms

makeLenses ''Dispatch
