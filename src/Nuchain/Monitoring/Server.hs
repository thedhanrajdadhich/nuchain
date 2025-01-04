{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- TODO: References to Prometheus (Ridley) have been removed, this will not function
-- until some replacement is found
module Nuchain.Monitoring.Server
  ( startMonitoring
  ) where

import Control.Lens ((^.), to)

import Data.Text.Encoding (decodeUtf8)
import qualified Data.ByteString.Base64 as B64
import qualified Data.Text as T
import qualified System.Metrics.Label as Label
import qualified System.Metrics.Gauge as Gauge
import qualified System.Metrics.Distribution as Dist

import Nuchain.Config.TMVar
import Nuchain.Util.Util (awsDashVar)
import Nuchain.Types (Metric(..), LogIndex(..), Term(..), NodeId(..), _port)
import Nuchain.Monitoring.EkgMonitor ( Server, forkServer, getLabel, getGauge, getDistribution)

startApi :: Config -> IO Server
startApi config = do
  let port = 80 + fromIntegral (config ^. nodeId . to _port)
  forkServer "localhost" port

startMonitoring :: Config -> IO (Metric -> IO ())
startMonitoring config = do
  ekg <- startApi config
  let awsDashVar' = awsDashVar False -- (config ^. enableAwsIntegration)

  -- Consensus
  termGauge <- getGauge "nuchain.consensus.term" ekg
  commitIndexGauge <- getGauge "nuchain.consensus.commit_index" ekg
  commitPeriodDist <- getDistribution "nuchain.consensus.commit_period" ekg
  currentLeaderLabel <- getLabel "nuchain.consensus.current_leader" ekg
  hashLabel <- getLabel "nuchain.consensus.hash" ekg
  -- Node
  nodeIdLabel <- getLabel "nuchain.node.id" ekg
  hostLabel <- getLabel "nuchain.node.host" ekg
  portGauge <- getGauge "nuchain.node.port" ekg
  roleLabel <- getLabel "nuchain.node.role" ekg
  appliedIndexGauge <- getGauge "nuchain.node.applied_index" ekg
  applyLatencyDist <- getDistribution "nuchain.node.apply_latency" ekg
  -- Cluster, quorum size
  clusterSizeGauge <- getGauge "nuchain.cluster.size" ekg
  quorumSizeGauge <- getGauge "nuchain.cluster.quorum_size" ekg
  availableSizeGauge <- getGauge "nuchain.cluster.available_size" ekg
  -- Cluster configuration change
  changeToClusterSizeGauge <- getGauge "nuchain.cluster.change_to_size" ekg
  changeToQuorumSizeGauge <- getGauge "nuchain.cluster.change_to_quorum_size" ekg
  -- Cluster membership
  clusterMembersLabel <- getLabel "nuchain.cluster.members" ekg

  return $ \case
    -- Consensus
    MetricTerm (Term t) -> do
      Gauge.set termGauge $ fromIntegral t
      awsDashVar' "Term" $ show t
    MetricCommitIndex (LogIndex idx) -> do
      Gauge.set commitIndexGauge $ fromIntegral idx
      awsDashVar' "CommitIndex" $ show idx
    MetricCommitPeriod p ->
      Dist.add commitPeriodDist p
    MetricCurrentLeader mNode ->
      case mNode of
        Just node -> Label.set currentLeaderLabel $ nodeDescription node
        Nothing -> Label.set currentLeaderLabel ""
    MetricHash bs ->
      Label.set hashLabel $ decodeUtf8 $ B64.encode bs
    -- Node
    MetricNodeId node@(NodeId host port _ _) -> do
      Label.set nodeIdLabel $ nodeDescription node
      Label.set hostLabel $ T.pack host
      Gauge.set portGauge $ fromIntegral port
    MetricRole role -> do
      Label.set roleLabel $ T.pack $ show role
      awsDashVar' "Role" $ show role
    MetricAppliedIndex (LogIndex idx) -> do
      Gauge.set appliedIndexGauge $ fromIntegral idx
      awsDashVar' "AppliedIndex" $ show idx
    MetricApplyLatency l ->
      Dist.add applyLatencyDist l
    -- Cluster
    MetricClusterSize size ->
      Gauge.set clusterSizeGauge $ fromIntegral size
    MetricQuorumSize size ->
      Gauge.set quorumSizeGauge $ fromIntegral size
    MetricAvailableSize size ->
      Gauge.set availableSizeGauge $ fromIntegral size
    -- Cluster configuration change
    MetricChangeToClusterSize size ->
      Gauge.set changeToClusterSizeGauge $ fromIntegral size
    MetricChangeToQuorumSize size ->
      Gauge.set changeToQuorumSizeGauge $ fromIntegral size
    MetricClusterMembers members ->
      Label.set clusterMembersLabel members
  where
    nodeDescription :: NodeId -> T.Text
    nodeDescription (NodeId host port _ _) = T.pack $ host ++ ":" ++ show port
