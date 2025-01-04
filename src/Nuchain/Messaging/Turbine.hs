module Nuchain.Messaging.Turbine
  ( runMessageReceiver
  ) where

import Control.Concurrent (takeMVar)
import Control.Lens
import Control.Monad
import Control.Monad.Reader

import Nuchain.Util.Util (foreverRetry)

import Nuchain.Messaging.Turbine.Util
import Nuchain.Messaging.Turbine.AER
import Nuchain.Messaging.Turbine.NewCMD
import Nuchain.Messaging.Turbine.General
import Nuchain.Messaging.Turbine.RV
import Nuchain.Types.Turbine (ReceiverEnv(..), turbineDebugPrint)

runMessageReceiver :: ReceiverEnv -> IO ()
runMessageReceiver env = void $ foreverRetry (env ^. turbineDebugPrint) "[Turbo|MsgReceiver]" $ runReaderT messageReceiver env

-- | Thread to take incoming messages and write them to the event queue.
messageReceiver :: ReaderT ReceiverEnv IO ()
messageReceiver = do
  env <- ask
  debug <- view turbineDebugPrint
  void $ liftIO $ foreverRetry debug turbineRv $ runReaderT rvAndRvrTurbine env
  void $ liftIO $ foreverRetry debug turbineAer $ runReaderT aerTurbine env
  void $ liftIO $ foreverRetry debug turbineCmd $ runReaderT newCmdTurbine env
  void $ liftIO $ foreverRetry debug turbineGeneral $ runReaderT generalTurbine env
  liftIO $ takeMVar (_restartTurbo env) >>= debug . (++) "restartTurbo MVar caught saying: "
