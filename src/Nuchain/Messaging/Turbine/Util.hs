module Nuchain.Messaging.Turbine.Util
  ( turbineRv
  , turbineAer
  , turbineCmd
  , turbineGeneral
  , parallelVerify
  , parSeqToList
  , processMsg
  ) where

import Control.Parallel.Strategies

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import Nuchain.Types.Crypto
import Nuchain.Message
import Nuchain.Types hiding (debugPrint)

turbineRv, turbineAer, turbineCmd, turbineGeneral :: String
turbineRv = "[Turbine|Rv]: "
turbineAer = "[Turbine|Aer]: "
turbineCmd = "[Turbine|Cmd]: "
turbineGeneral = "[Turbine|Gen]: "

parallelVerify :: (f -> (ReceivedAt,SignedRPC)) -> KeySet -> Seq f -> [Either String RPC]
parallelVerify f ks msgs = runEval $ parSeqToList (processMsg f ks) msgs

parSeqToList :: (f -> Either String RPC) -> Seq f -> Eval [Either String RPC]
parSeqToList f s = do
  case Seq.viewl s of
    Seq.EmptyL -> return []
    x Seq.:< xs -> do
      res <- rpar (f x)
      (res:) <$> parSeqToList f xs

processMsg :: (f -> (ReceivedAt,SignedRPC)) -> KeySet -> (f -> Either String RPC)
processMsg f ks = (\(ts, msg) -> signedRPCtoRPC (Just ts) ks msg) . f
