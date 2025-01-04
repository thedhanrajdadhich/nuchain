{-# LANGUAGE AllowAmbiguousTypes #-}

module Nuchain.ConfigChange.Util
  ( getMissingKeys
  ) where

import qualified Data.Map as Map

import Nuchain.Config.TMVar
import qualified Nuchain.Types.Crypto as KC
import Nuchain.Types.Base
import Nuchain.Types.Command (CCPayload(..))

getMissingKeys :: Config -> CCPayload -> IO [Alias]
getMissingKeys cfg payload = do
  let signerKeys = KC._siPubKey <$> _ccpSigners payload
  let filtered = filter f (Map.toList (_adminKeys cfg)) where
        f (_, k) = notElem k signerKeys
  return $ fmap fst filtered
