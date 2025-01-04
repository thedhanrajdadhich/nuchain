{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Nuchain.PreProc.Service
  ( initPreProcEnv
  , runPreProcService
  ) where

import Control.Lens hiding (Index, (|>))
import Control.Monad
import Control.DeepSeq
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Concurrent.Async
import Control.Parallel.Strategies

import Data.Ratio
import Data.AffineSpace
import Data.Thyme.Clock
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Foldable

import qualified Nuchain.Config.TMVar as Cfg
import Nuchain.Types.Comms
import Nuchain.Types.Command (FinishedPreProc(..))
import Nuchain.Types.PreProc
import Nuchain.Types.Dispatch (Dispatch)
import qualified Nuchain.Types.Dispatch as D
import Nuchain.Event
import Nuchain.Command (runPreproc, runPreprocPure, finishPreProc)
import Nuchain.Types.Event (Beat)

initPreProcEnv
  :: Dispatch
  -> Int
  -> (String -> IO ())
  -> IO UTCTime
  -> Bool
  -> Cfg.GlobalConfigTMVar
  -> ProcessRequestEnv
initPreProcEnv dispatch' threadCount' debugPrint' getTimestamp' usePar' gCfg' = ProcessRequestEnv
  { _preProcessRequestChannel = dispatch' ^. D.dispProcessRequestChannel
  , _preThreadCount = threadCount'
  , _preDebugPrint = debugPrint'
  , _preGetTimestamp = getTimestamp'
  , _preUsePar = usePar'
  , _preConfig = gCfg'
  }

runPreProcService :: ProcessRequestEnv -> IO ()
runPreProcService env = do
  let dbg = env ^. preDebugPrint
  dbg "[Service|PreProc] Launch!"
  if env ^. preUsePar
  then do
    viaPar env
  else do
    threads' <- threadPool env
    mapM_ link threads'
    waitAnyCatchCancel threads' >>= \case
      (_,shouldBeUnreachable) -> error $ "unreachable exception reached in preproc... " ++ show shouldBeUnreachable

debug :: String -> ProcessRequestService ()
debug s = do
  when (not (null s)) $ do
    dbg <- view preDebugPrint
    liftIO $! dbg $ "[Service|PreProc] " ++ s

now :: ProcessRequestService UTCTime
now = view preGetTimestamp >>= liftIO

threadPool :: ProcessRequestEnv -> IO [Async ()]
threadPool env@ProcessRequestEnv{..} = replicateM _preThreadCount $
  async $ forever $ runReaderT (handle _preProcessRequestChannel) env

ppBeat :: Beat -> ProcessRequestService ()
ppBeat b = do
  gCfg <- view preConfig
  conf <- liftIO $ Cfg.readCurrentConfig gCfg
  liftIO (pprintBeat b conf) >>= debug

handle :: ProcessRequestChannel -> ProcessRequestService ()
handle workChan = do
  liftIO (readComm workChan) >>= \case
    (CommandPreProc rpp) -> do
      hitPreProc <- now
      liftIO $! void $! runPreproc hitPreProc rpp
    PreProcBeat t -> ppBeat t
{-# INLINE handle #-}

betterParallelProc :: NFData a => [a] -> [a]
betterParallelProc xs = runEval $ do
  pared <- mapM (rparWith rdeepseq) xs
  mapM (rseq) pared

handleCmdPar :: UTCTime -> Seq ProcessRequest -> ProcessRequestService ()
handleCmdPar startTime s = do
  let asList = toList s
  res <- return $! betterParallelProc $ evalPreProcCmd <$> asList
  endTime <- now >>= return . mkProperTime (Seq.length s) startTime
  mapM_ (liftIO . finishPreProc startTime endTime) res

mkProperTime :: Int -> UTCTime -> UTCTime -> UTCTime
mkProperTime cnt startTime endTime = properTime
  where
    delta = (endTime .-. startTime) ^. microseconds
    properTime = startTime .+^ (view (from microseconds) (round $ delta % (fromIntegral cnt)))
{-# INLINE mkProperTime #-}

evalPreProcCmd :: ProcessRequest -> FinishedPreProc
evalPreProcCmd (CommandPreProc rpp) = runPreprocPure rpp
evalPreProcCmd PreProcBeat{} = error $ "Invariant Error: `evalPreProcCmd` caught a HeartBeat"
{-# INLINE evalPreProcCmd #-}

filterBatch :: Seq ProcessRequest -> (Seq ProcessRequest, Seq ProcessRequest)
filterBatch s = Seq.partition isHB s
  where
    isHB :: ProcessRequest -> Bool
    isHB (PreProcBeat _) = True
    isHB (CommandPreProc _) = False
{-# INLINE filterBatch #-}

viaPar :: ProcessRequestEnv -> IO ()
viaPar env@ProcessRequestEnv{..} = do
  let getWork = filterBatch <$> readComms _preProcessRequestChannel _preThreadCount
  (flip runReaderT) env $ forever $ do
    (hbs, newWork) <- liftIO $ getWork
    unless (Seq.null hbs) $ forM_ hbs $ \case
      PreProcBeat t -> ppBeat t
      CommandPreProc{} -> error $ "Invariant Error: `viaPar` caught a CommandPreProc"
    unless (Seq.null newWork) $ do
      startTime <- now
      handleCmdPar startTime newWork
{-# INLINE viaPar #-}
