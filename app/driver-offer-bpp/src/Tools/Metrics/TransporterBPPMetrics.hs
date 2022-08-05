{- |
Copyright 2022 Juspay Technologies Pvt Ltd

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

Module      :  Tools.Metrics.TransporterBPPMetrics

Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Tools.Metrics.TransporterBPPMetrics
  ( module Tools.Metrics.TransporterBPPMetrics,
    module Reexport,
  )
where

import Beckn.Types.Amount
import Beckn.Types.Common (Milliseconds, getSeconds)
import Beckn.Types.Id
import Beckn.Utils.Time (getClockTimeInMs)
import Domain.Types.Organization
import qualified EulerHS.Language as L
import EulerHS.Prelude
import GHC.Records.Extra
import Prometheus as P
import Tools.Metrics.TransporterBPPMetrics.Types as Reexport
import Utils.Common (Forkable (fork), MonadFlow)

putFareAndDistanceDeviations :: (MonadMonitor m, HasTransporterMetrics m r) => Amount -> Double -> m ()
putFareAndDistanceDeviations fareDiff distanceDiff = do
  TransporterMetricsContainer {..} <- asks (.transporterMetrics)
  P.observe realFareDeviation $ amountToDouble fareDiff
  P.observe realDistanceDeviation distanceDiff

type SearchMetricsMVar = MVar Milliseconds

startSearchMetrics :: HasBPPMetrics m r => Id Organization -> m SearchMetricsMVar
startSearchMetrics transporterId = do
  bmContainer <- asks (.bppMetrics)
  startSearchMetrics' transporterId bmContainer

finishSearchMetrics :: HasBPPMetrics m r => Id Organization -> SearchMetricsMVar -> m ()
finishSearchMetrics transporterId searchMetricsMVar = do
  bmContainer <- asks (.bppMetrics)
  finishSearchMetrics' transporterId bmContainer searchMetricsMVar

putSearchDuration :: L.MonadFlow m => Id Organization -> P.Vector P.Label1 P.Histogram -> Double -> m ()
putSearchDuration transporterId searchDurationHistogram duration =
  L.runIO $
    P.withLabel
      searchDurationHistogram
      (show transporterId)
      (`P.observe` duration)

startSearchMetrics' :: MonadFlow m => Id Organization -> BPPMetricsContainer -> m SearchMetricsMVar
startSearchMetrics' transporterId bmContainer = do
  let (_, failureCounter) = bmContainer.searchDuration
      searchDurationTimeout = getSeconds bmContainer.searchDurationTimeout
  startTime <- getClockTimeInMs
  searchMetricsMVar <- liftIO $ newMVar startTime
  fork "BPP Search Metrics" $ do
    liftIO $ threadDelay $ searchDurationTimeout * 1000000
    whenJustM (liftIO $ tryTakeMVar searchMetricsMVar) $ \_ -> do
      liftIO $ P.withLabel failureCounter (show transporterId) P.incCounter
  return searchMetricsMVar

finishSearchMetrics' ::
  MonadFlow m =>
  Id Organization ->
  BPPMetricsContainer ->
  SearchMetricsMVar ->
  m ()
finishSearchMetrics' transporterId bmContainer searchMetricsMVar = do
  let (searchDurationHistogram, _) = bmContainer.searchDuration
  whenJustM (liftIO $ tryTakeMVar searchMetricsMVar) $ \startTime -> do
    endTime <- getClockTimeInMs
    putSearchDuration transporterId searchDurationHistogram $ fromIntegral $ endTime - startTime
