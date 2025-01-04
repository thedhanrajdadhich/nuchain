{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Nuchain.Types.PreProc
  ( ProcessRequest(..)
  , ProcessRequestEnv(..)
  , preProcessRequestChannel, preDebugPrint, preGetTimestamp, preThreadCount, preUsePar, preConfig
  , ProcessRequestChannel(..)
  , ProcessRequestService
  ) where

import Control.Lens hiding (Index)

import Control.Monad.Trans.Reader
import Control.Concurrent.Chan (Chan)
import Control.Concurrent.STM

import Data.Sequence (Seq)
import Data.Thyme.Clock (UTCTime)

import Nuchain.Config.TMVar as Cfg
import Nuchain.Types.Command
import Nuchain.Types.Comms
import Nuchain.Types.Event (Beat)

data ProcessRequest =
  CommandPreProc
  { _cmdPreProc :: !RunPreProc } |
  PreProcBeat Beat

newtype ProcessRequestChannel = ProcessRequestChannel (Chan ProcessRequest, TVar (Seq ProcessRequest))

instance Comms ProcessRequest ProcessRequestChannel where
  initComms = ProcessRequestChannel <$> initCommsBatched
  readComm (ProcessRequestChannel (_,m)) = readCommBatched m
  writeComm (ProcessRequestChannel (c,_)) = writeCommBatched c
  {-# INLINE initComms #-}
  {-# INLINE readComm #-}
  {-# INLINE writeComm #-}

instance BatchedComms ProcessRequest ProcessRequestChannel where
  readComms (ProcessRequestChannel (_,m)) cnt = readCommsBatched m cnt
  {-# INLINE readComms #-}
  writeComms (ProcessRequestChannel (_,m)) xs = writeCommsBatched m xs
  {-# INLINE writeComms #-}

data ProcessRequestEnv = ProcessRequestEnv
  { _preProcessRequestChannel :: !ProcessRequestChannel
  , _preThreadCount :: !Int
  , _preDebugPrint :: !(String -> IO ())
  , _preGetTimestamp :: !(IO UTCTime)
  , _preUsePar :: !Bool
  , _preConfig :: !Cfg.GlobalConfigTMVar
  }
makeLenses ''ProcessRequestEnv

type ProcessRequestService = ReaderT ProcessRequestEnv IO
