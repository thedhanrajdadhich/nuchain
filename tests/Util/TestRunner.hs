{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Util.TestRunner
  ( delTempFiles
  , gatherMetric
  , gatherMetric'
  , testDir
  , testConfDir
  , runClientCommands
  , runServers
  , runServers'
  , TestMetric(..)
  , TestMetricResult(..)
  , TestRequest(..)
  , TestResponse(..)
  , TestResult(..)) where

import           Apps.Nuchain.Client
import qualified Apps.Nuchain.Server as App
import           Control.Concurrent
import           Control.Lens
import           Control.Monad
import           Control.Monad.Trans.RWS.Lazy
import qualified Crypto.Ed25519.Pure as Ed25519
import           Data.Aeson hiding (Success)
import qualified Data.ByteString.Lazy.Char8 as C8
import           Data.Default
import qualified Data.HashMap.Strict as HM
import           Data.Int
import           Data.List
import           Data.List.Extra
import qualified Data.Yaml as Y
import           GHC.Generics (Generic)
import           Nuchain.Types.Command (CommandResult(..))
import           Network.Wreq
import qualified Network.Wreq as WR (getWith)
import qualified Pact.ApiReq as Pact
import qualified Pact.Types.Crypto as Pact
import           System.Console.GetOpt
import           System.Environment
import           System.Exit
import           System.Process
import           System.Time.Extra
import           Test.Hspec
import           Text.Trifecta (ErrInfo(..), parseString, Result(..))

testDir, testConfDir, _testLogDir :: String
testDir = "test-files/"
testConfDir = "test-files/conf/"
_testLogDir = "test-files/log/"

data TestRequest = TestRequest
  { cmd :: String
  , matchCmd :: String -- used when the command as processed differs from the original command issued
                       -- e.g., the command "load myFile.yaml" is processed as "myFile.yaml"
                       -- FIXME: really need to find a better way to match these...
  , eval :: TestResponse -> Expectation
  , displayStr :: String
  }

instance Show TestRequest where
  show tr = "cmd: " ++ cmd tr ++ "\nDisplay string: " ++ displayStr tr

data TestResponse = TestResponse
  { resultSuccess :: Bool
  , apiResult :: CommandResult
  , _batchCount :: Int64
  } deriving (Eq, Generic)

instance Show TestResponse where
  show tr = "lesultSuccess: " ++ show (resultSuccess tr) ++ "\n"
    ++ "Batch count: " ++ show (_batchCount tr) ++ "\n"
    ++ take 100 (show (apiResult tr)) ++ "..."

data TestResult = TestResult
  { requestTr :: TestRequest
  , responseTr :: Maybe TestResponse
  } deriving Show

-- TODO Make `metricNameTm` a path type from `paths`
data TestMetric = TestMetric
  { testNameTm :: String
  , metricNameTm :: String
  , evalTm :: String -> Bool
  }
instance Show TestMetric where
  show tm = show $ metricNameTm tm

data TestMetricResult = TestMetricResult
  { requestTmr :: TestMetric
  , valueTmr :: Maybe String
  } deriving Show

delTempFiles :: IO ()
delTempFiles = do
    let p = shell $ testDir ++ "deleteFiles.sh"
    _ <- createProcess p
    return ()

-- | Returns a list of IO actions that kill all the servers
runServers :: IO ()
runServers = runServers' serverArgs

runServers' :: [String] -> IO ()
runServers' svrArgList = do
  sleep 1
  mapM_ runServer svrArgList

-- | Returns an IO action that kills the thread.
runServer ::  String -> IO ()
runServer args = do
    _ <- forkIO (withArgs (words args) App.main)
    sleep 1
    return ()

serverArgs :: [String]
serverArgs = [serverArgs0, serverArgs1, serverArgs2, serverArgs3]

serverArgs0, serverArgs1, serverArgs2, serverArgs3 :: String
serverArgs0 = "-c " ++ testConfDir ++ "10000-cluster.yaml"
serverArgs1 = "-c " ++ testConfDir ++ "10001-cluster.yaml"
serverArgs2 = "-c " ++ testConfDir ++ "10002-cluster.yaml"
serverArgs3 = "-c " ++ testConfDir ++ "10003-cluster.yaml"

runClientCommands :: [String] ->  [TestRequest] -> IO [TestResult]
runClientCommands args testRequests = do
  case getOpt Permute coptions args of
    (_,_,es@(_:_)) -> print es >> exitFailure
    (o,_,_) -> do
      let opts = foldl (flip id) def o
      i <- newMVar =<< initRequestId
      (conf :: ClientConfig) <- either (\e -> print e >> exitFailure) return
        =<< Y.decodeFileEither (_oConfig opts)
      let replState = ReplState
            { _server = fst (minimum $ HM.toList (_ccEndpoints conf))
            , _batchCmd = "\"Hello Nuchain\""
            , _requestId = i
            , _cmdData = Null
            , _keys = Just [Pact.ApiKeyPair
                           (Pact.PrivBS (Ed25519.exportPrivate (_ccSecretKey conf)))
                           (Just (Pact.PubBS (Ed25519.exportPublic (_ccPublicKey conf))))
                           Nothing
                           Nothing
                           Nothing]
            , _fmt = Table
            , _echo = False }
      let server = getServer conf replState
      clientEnv <- getClientEnv server
      let replConfig = ReplConfig
            { _rcClientConfig = conf
            , _rcClientEnv = clientEnv
            }
      (_, _, w) <- runRWST (simpleRunREPL testRequests) replConfig replState
      buildResults testRequests w

buildResults :: [TestRequest] -> [ReplApiData] -> IO [TestResult]
buildResults testRequests ys = do
  let requests = filter isRequest ys
  let responses = filter (not . isRequest) ys
  let matched = foldr (matchResponses requests responses) [] testRequests
  let outStr = concatMap show matched
  putStrLn $ "Matched results from buildResults: " ++ outStr
  return matched

-- Fold function that matches a given TestRequest to:
--   a corresponding ReplApiRequest (matching via. the full text of the command)
--   a corresponding ReplApiResponse (matching via. requestKey))
-- and then builds a TestResult combining elements from both
matchResponses :: [ReplApiData] -> [ReplApiData] -> TestRequest -> [TestResult] -> [TestResult]
matchResponses [] _ _ acc = acc -- no requests
matchResponses _ [] _ acc = acc -- no responses
matchResponses requests@(ReplApiRequest _ _ : _)
               responses@(ReplApiResponse{}: _)
               testRequest acc =
  let theApiRequest = find (\req -> _replCmd req == matchCmd testRequest) requests
      theApiResponse = case theApiRequest of
        Nothing -> Nothing
        Just req -> find (\resp -> _apiResponseKey resp == _apiRequestKey req) responses
      testResponse =  theApiResponse >>= convertResponse
  in case testResponse of
    Just _ -> TestResult
                { requestTr = testRequest
                , responseTr = testResponse
                } : acc
    Nothing -> acc
matchResponses _ _ _ acc = acc -- this shouldn't happen

convertResponse :: ReplApiData -> Maybe TestResponse
convertResponse (ReplApiResponse _ cr batchCnt) =
  Just TestResponse { resultSuccess = True
                     , apiResult = cr
                     , _batchCount = batchCnt }
convertResponse _ = Nothing  -- this shouldn't happen

_printResponses :: [ReplApiData] -> IO ()
_printResponses xs =
  forM_ xs printResponse where
    printResponse :: ReplApiData -> IO ()
    printResponse (ReplApiResponse _ cr batchCnt) = do
      putStrLn $ "Batch count: " ++ show batchCnt
      putStrLn "\n***** printResponse *****"
      print $ cr
    printResponse _ = return ()

isRequest :: ReplApiData -> Bool
isRequest (ReplApiRequest _ _) = True
isRequest ReplApiResponse{} = False

simpleRunREPL :: [TestRequest] -> Repl ()
simpleRunREPL [] = return ()
simpleRunREPL (x:xs) = do
  let reqStr = cmd x
  case parseString parseCliCmd mempty reqStr of
    Failure (ErrInfo e _) -> do
      flushStrLn $ "Parse failure (help for command help):\n" ++ show e
      return ()
    Success c -> do
      handleCmd c reqStr
      simpleRunREPL xs

gatherMetric :: TestMetric -> IO TestMetricResult
gatherMetric tm = gatherMetric' tm 0

-- Version of gatherMetric that takes a node number (0-3) as additional param
gatherMetric' :: TestMetric -> Int -> IO TestMetricResult
gatherMetric' tm node = do
    let name = metricNameTm tm
    value <- getMetric name node
    return $ TestMetricResult { requestTmr = tm, valueTmr = Just value}

getMetric :: String -> Int -> IO String
getMetric path node = do
  let opts = defaults & header "Accept" .~ ["application/json"]
  let portStr = show $ (10080 + node)
  rbs <- WR.getWith opts $ "http://localhost:" ++ portStr ++ path
  let str = C8.unpack $ rbs ^. responseBody
      val = takeWhile (/= '}') $ takeWhileEnd (/= ':') str
  pure val
