{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Nuchain.Types.Message.AE
  ( AppendEntries(..), aeTerm, leaderId, prevLogIndex, prevLogTerm, aeEntries, aeQuorumVotes,
    aeProvenance
  , AEWire(..)
  ) where

import Codec.Compression.LZ4
import Control.Lens
import Control.Parallel.Strategies
import Data.Serialize (Serialize)
import qualified Data.Serialize as S
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Thyme.Time.Core ()
import GHC.Generics

import Nuchain.Types.Crypto (sign)
import Nuchain.Log
import Nuchain.Types.Base
import Nuchain.Types.Message.Signed
import Nuchain.Types.Message.RVR
import Nuchain.Types.Log

import qualified Pact.Types.Hash as P (pactHash)

data AppendEntries = AppendEntries
  { _aeTerm        :: !Term
  , _leaderId      :: !NodeId
  , _prevLogIndex  :: !LogIndex
  , _prevLogTerm   :: !Term
  , _aeEntries     :: !LogEntries
  , _aeQuorumVotes :: !(Set RequestVoteResponse)
  , _aeProvenance  :: !Provenance
  }
  deriving (Show, Eq, Generic)
makeLenses ''AppendEntries

data AEWire = AEWire (Term,NodeId,LogIndex,Term,[LEWire],[SignedRPC])
  deriving (Show, Generic)
instance Serialize AEWire

instance WireFormat AppendEntries where
  toWire nid pubKey privKey AppendEntries{..} = case _aeProvenance of
    ReceivedMsg{..} -> SignedRPC _pDig _pOrig
    NewMsg ->
      let ae = S.encode $ AEWire ( _aeTerm
                                  , _leaderId
                                  , _prevLogIndex
                                  , _prevLogTerm
                                  , encodeLEWire _aeEntries
                                  , toWire nid pubKey privKey <$> Set.toList _aeQuorumVotes )
          bdy = fromMaybe ae (compressHC ae)
          hsh = P.pactHash bdy
          sig = sign hsh privKey pubKey
          dig = Digest (_alias nid) sig pubKey AE hsh
      in SignedRPC dig bdy

  fromWire !ts !ks s@(SignedRPC !dig !bdy) = case verifySignedRPC ks s of
    Left !err -> Left err
    Right () -> if _digType dig /= AE
      then error $ "Invariant Failure: attempting to decode " ++ show (_digType dig) ++ " with AEWire instance"
      else case maybe (Left "Decompression failure") S.decode $ decompress bdy of
        Left err -> Left $! "Failure to decode AEWire: " ++ err
        Right (AEWire (t,lid,pli,pt,les,vts)) -> runEval $ do
          eLes <- rpar (toLogEntries ((decodeLEWire' ts <$> les) `using` parList rseq))
          eRvr <- rseq (toSetRvr ((fromWire ts ks <$> vts) `using` parList rseq))
          case eRvr of
            Left !err -> return $! Left $! "Caught an invalid RVR in an AE: " ++ err
            Right !vts' -> do
              _ <- rseq eLes
              return $! Right $! AppendEntries t lid pli pt eLes vts' $ ReceivedMsg dig bdy ts
  {-# INLINE toWire #-}
  {-# INLINE fromWire #-}
