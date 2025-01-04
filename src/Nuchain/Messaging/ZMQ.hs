{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Nuchain.Messaging.ZMQ (
  runMsgServer
  ) where

import Control.Lens
import Control.Concurrent
import qualified Control.Concurrent.Async as Async
import Control.Monad.State.Strict
import System.ZMQ4.Monadic
import Data.Thyme.Clock
import Data.Serialize
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String.Conv (toS)

import Nuchain.Message
import Nuchain.Types
import Nuchain.Util.Util (catchAndRethrow, linkAsyncTrack)
import qualified Nuchain.Config.ClusterMembership as CM
import Nuchain.Config.TMVar
import Nuchain.ConfigChange

data Shutdown = IsShutdown | IsPending
data ReconfSub = ReconfSub
  { _mNewNodeList :: !(Maybe (Set NodeId))
  , _mShutdownSub :: !(Maybe (MVar Shutdown)) }

nodeIdToZmqAddr, myInterface :: NodeId -> String
nodeIdToZmqAddr NodeId{..} = "tcp://" ++ _host ++ ":" ++ show _port
myInterface NodeId{..} = "tcp://0.0.0.0:" ++ show _port

zmqLinkedAsync :: String -> ZMQ z a -> ZMQ z ()
zmqLinkedAsync loc fn = do
  a <- async $ catchAndRethrow loc fn
  liftIO $ Async.link a

zmqPub, zmqSub :: String
zmqPub = "[Zmq|Pub]: "
zmqSub = "[Zmq|Sub]: "

runMsgServer :: Dispatch
             -> NodeId
             -> [NodeId]
             -> (String -> IO ())
             -> GlobalConfigTMVar
             -> IO ()
runMsgServer dispatch me addrList debug gcm = forever $ do
  inboxWrite <- return $ dispatch ^. dispInboundGeneral
  cmdInboxWrite <- return $ dispatch ^. dispInboundCMD
  aerInboxWrite <- return $ dispatch ^. dispInboundAER
  rvAndRvrWrite <- return $ dispatch ^. dispInboundRVorRVR
  outboxRead <- return $ dispatch ^. dispOutboundGeneral

  semephory <- newEmptyMVar -- this MVar is for coordinating the lighting of ZMQ. There's an annoying segfault/malloc error that I think is caused by ZMQ.
  shutdownPub <- newEmptyMVar
  shutdownSub <- newEmptyMVar
  reconfigureSub <- newEmptyMVar

  linkAsyncTrack "runZMQ" $ runZMQ $ do
    -- ZMQ Pub Thread
    zmqLinkedAsync "ZmqPub" $ do
      liftIO $ debug $ zmqPub ++ "launch!"
      pubSock <- socket Pub
      _ <- bind pubSock $ myInterface me
      liftIO $ putMVar semephory ()
      let
        runPub = liftIO (tryTakeMVar shutdownPub) >>= \case
          Nothing -> do
            !msgs <- liftIO (_unOutboundGeneral <$> readComm outboxRead) >>= return . fmap sealEnvelope
            startTime <- liftIO getCurrentTime
            mapM_ (sendMulti pubSock) msgs
            endTime <- liftIO getCurrentTime
            liftIO $ debug $ zmqPub ++ "publishing msg " ++ (printInterval startTime endTime)
            runPub
          Just _ -> do
            close pubSock
            liftIO $ debug $ zmqPub ++ "shutting down"
            liftIO $ putMVar shutdownPub IsShutdown
      runPub


    liftIO $ void $ takeMVar semephory

    zmqLinkedAsync "ZmqSub" $ do
      subSocket <- socket Sub
      subscribe subSocket "all" -- the topic for broadcast messages
      liftIO $ debug $ zmqSub ++ "subscribed to: \"all\""
      subscribe subSocket $ toS $ unAlias $ _alias me
      liftIO $ debug $ zmqSub ++ "subscribed to: " ++ show (unAlias $ _alias me)
      void $ forM_ addrList $ \addr -> do
          _ <- connect subSocket $ nodeIdToZmqAddr $ addr
          liftIO $ debug $ zmqSub ++ "connected to: " ++ (show $ nodeIdToZmqAddr addr)
      connectedNodeIdsMV <- liftIO $ newMVar $ Set.fromList addrList
      liftIO $ putMVar semephory ()
      let
        runSub = liftIO (tryTakeMVar reconfigureSub) >>= \case
          Just (ReconfSub _ (Just shutdownSubMV)) ->
            liftIO $ tryTakeMVar shutdownSubMV >>= \case
              Nothing -> error "invariant error: shutdownSubMV should be populated"
              Just _ -> putMVar shutdownSubMV IsShutdown
          Just (ReconfSub (Just newNodeList) _) -> do
            connectedNodeIds <- liftIO $ takeMVar connectedNodeIdsMV
            DiffNodes{..} <- return $ diffNodes connectedNodeIds newNodeList
            void $ forM_ nodesToAdd $ \addr -> do
              _ <- connect subSocket $ nodeIdToZmqAddr $ addr
              let connectStr = zmqSub ++ "CC - connecting to: " ++ (show $ nodeIdToZmqAddr addr)
              liftIO $ putStrLn connectStr -- remove this eventually, too useful for now...
              liftIO $ debug connectStr
            void $ forM_ nodesToRemove $ \addr -> do
              _ <- disconnect subSocket $ nodeIdToZmqAddr $ addr
              let deconStr = zmqSub ++ "disconnected from: " ++ (show $ nodeIdToZmqAddr addr)
              liftIO $ debug deconStr
              liftIO $ putStrLn deconStr -- remove this eventually, too useful for now...
            liftIO $ putMVar connectedNodeIdsMV newNodeList
            liftIO $ debug $ zmqSub ++ "reconfigured ZMQ"
            runSub
          Just (ReconfSub Nothing Nothing) -> do
            liftIO $ debug $ zmqSub ++ "ERROR: ReconfSub Nothing Nothing"
            runSub
          Nothing -> do
            env <- openEnvelope <$> receiveMulti subSocket
            ts <- liftIO getCurrentTime
            case env of
              Left err ->
                liftIO $ debug $  zmqSub ++ show err
              Right (Envelope (_topic',newMsg)) -> do
                liftIO $ debug $  zmqSub ++ "got msg on topic: " ++ show (_unTopic _topic')
                case decode newMsg of
                  Left err -> do
                    liftIO $ debug $ zmqSub ++ "failed to deserialize to SignedRPC [Msg]: " ++ show newMsg
                    liftIO $ debug $ zmqSub ++ "failed to deserialize to SignedRPC [Error]: " ++ err
                    liftIO yield
                  Right s@(SignedRPC dig _)
                    | _digType dig == RV || _digType dig == RVR -> do
                      endTime <- liftIO getCurrentTime
                      liftIO $ writeComm rvAndRvrWrite (InboundRVorRVR (ReceivedAt ts, s)) >> yield
                      liftIO $ debug $ zmqSub ++ " Received RVR from: " ++ (show $ _digNodeId dig) ++ " " ++ printInterval ts endTime
                    | _digType dig == NEW -> do
                      endTime <- liftIO getCurrentTime
                      liftIO $ writeComm cmdInboxWrite (InboundCMD (ReceivedAt ts, s)) >> yield
                      liftIO $ debug $ zmqSub ++ " Received NEW from: " ++ (show $ _digNodeId dig) ++ " " ++ printInterval ts endTime
                    | _digType dig == AER -> do
                      endTime <- liftIO getCurrentTime
                      liftIO $ writeComm aerInboxWrite (InboundAER (ReceivedAt ts, s)) >> yield
                      liftIO $ debug $ zmqSub ++ " Received AER from: " ++ (show $ _digNodeId dig) ++ " " ++ printInterval ts endTime
                    | otherwise           -> do
                      endTime <- liftIO getCurrentTime
                      liftIO $ writeComm inboxWrite (InboundGeneral (ReceivedAt ts, s)) >> yield
                      liftIO $ debug $ zmqSub ++ " Received " ++ (show $ _digType dig) ++ " from " ++ (show $ _digNodeId dig) ++ " " ++ printInterval ts endTime
                runSub
      runSub
  let cu = ConfigUpdater debug "ZMQ|Config" (confUpdater reconfigureSub shutdownPub shutdownSub me)
  linkAsyncTrack "ZMQConfigUpdater" $ runConfigUpdater cu gcm

  void $ takeMVar shutdownPub
  void $ takeMVar shutdownSub

confUpdater :: MVar ReconfSub -> MVar Shutdown -> MVar Shutdown -> NodeId -> Config -> IO ()
confUpdater reconfMV shutdownPubMV shutdownSubMV me Config{..} = do
  let shouldShutdown = False
  if shouldShutdown
  then do
    putMVar shutdownPubMV IsPending
    putMVar shutdownSubMV IsPending
    putMVar reconfMV $ ReconfSub Nothing $ Just shutdownSubMV
  else do
    let theNodes = CM.getAllExcluding _clusterMembers me
    putMVar reconfMV $ ReconfSub (Just theNodes) Nothing
