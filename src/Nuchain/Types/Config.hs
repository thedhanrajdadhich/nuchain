{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}

module Nuchain.Types.Config
  ( ConfigUpdater(..)
  , DiffNodes(..)
  , GlobalConfigTMVar
  ) where

import Data.Set (Set)
import Data.Thyme.Time.Core ()
import GHC.Generics

import Nuchain.Types.Base
import Nuchain.Config.TMVar

data ConfigUpdater = ConfigUpdater
  { _cuPrintFn :: !(String -> IO ())
  , _cuThreadName :: !String
  , _cuAction :: (Config -> IO()) }

data DiffNodes = DiffNodes
  { nodesToAdd :: !(Set NodeId)
  , nodesToRemove :: !(Set NodeId)
  } deriving (Show,Eq,Ord,Generic)
