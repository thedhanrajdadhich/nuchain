{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module ConfigSpec where

import Data.Aeson
import qualified Data.Set as S
import qualified Data.HashSet as HS
import qualified Data.Map.Strict as M
import qualified Data.HashMap.Strict as HM
import "crypto-api" Crypto.Random
import qualified Crypto.Ed25519.Pure as Ed25519

import qualified Nuchain.Config.ClusterMembership as CM
import Nuchain.Types.PactDB
import Nuchain.Config.TMVar
import Nuchain.Types.Base
import Nuchain.Types.Entity as EN

import Pact.Types.Logger

import Test.Hspec

spec :: Spec
spec =
  describe "testConfigRT" $ testConfigRT

makeEdKeys :: CryptoRandomGen g => Int -> g -> [(Ed25519.PrivateKey, Ed25519.PublicKey)]
makeEdKeys 0 _ = []
makeEdKeys n g = case Ed25519.generateKeyPair g of
  Left err -> error $ show err
  Right (s,p,g') -> (s,p) : makeEdKeys (n-1) g'

dummyConfig :: IO Config
dummyConfig = do
  [(as,ap),(_bs,bp),(_cs,cp)] <- makeEdKeys 3 <$> (newGenIO :: IO SystemRandom)

  aStatic <- EN.genKeyPair
  aEph <- EN.genKeyPair
  bStatic <- EN.genKeyPair
  cSigner <- EN.genKeyPair

  let aRemote = EntityRemote "A" (ekPublic aStatic)
  let bRemote = EntityRemote "B" (ekPublic bStatic)

  return $ Config
    { _clusterMembers = CM.mkClusterMembership
        (S.fromList [ NodeId "hostB" 8001 "hostB:8001" "B" , NodeId "hostC" 8002 "hostB:8002" "C"])
        S.empty
    , _nodeId               = NodeId "hostA" 8000 "hostA:8000" "A"
    , _publicKeys           = M.fromList [("hostB",bp),("hostC",cp)]
    , _adminKeys            = M.fromList [("hostA",ap),("hostC",cp)]
    , _myPrivateKey         = as
    , _myPublicKey          = ap
    , _electionTimeoutRange = (6,7)
    , _heartbeatTimeout     = 5
    , _enableDebug          = True
    , _apiPort              = 4
    , _entity               = EntityConfig
        { _ecLocal = EntityLocal "A" aStatic aEph
        , _ecRemotes = [aRemote, bRemote]
        , _ecSending = True
        , _ecSigner = cSigner
        }
    , _logDir               = "/tmp/foo"
    , _enablePersistence    = False
    , _pactPersist          = PactPersistConfig
        { _ppcWriteBehind = False
        , _ppcBackend = PPBInMemory }
    , _aeBatchSize          = 2
    , _preProcThreadCount   = 3
    , _preProcUsePar        = True
    , _inMemTxCache         = 1
    , _hostStaticDir        = False
    , _nodeClass            = Active
    , _logRules             = LogRules $ HM.fromList
                              [("Foo", LogRule
                                       { enable = Just True,
                                         include = Nothing,
                                         exclude = Just (HS.singleton "DEBUG")
                                       })]

    , _enableDiagnostics    = Nothing
    }

testConfigRT :: Spec
testConfigRT = do
  c <- runIO $ dummyConfig
  let cenc = encode c
  case eitherDecode cenc :: Either String Config of
    Left err -> it "decodes" $ expectationFailure $ "decode failed: " ++ err
    Right cdec -> it "roundtrips" $ encode cdec `shouldBe` cenc
