module Nuchain.Types.ConfigChange
  ( ConfigChange (..)
  , ConfigChangeException (..)
  ) where

import Control.Monad.Catch
import Data.Set (Set)
import Nuchain.Types.Base

data ConfigChange = ConfigChange
  { newNodeSet :: !(Set NodeId)
  , consensusLists :: ![Set NodeId]
  } deriving Eq

newtype ConfigChangeException = ConfigChangeException String
  deriving (Eq,Show,Ord)
instance Exception ConfigChangeException
