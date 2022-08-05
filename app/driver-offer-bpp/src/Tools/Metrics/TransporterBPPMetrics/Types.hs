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

Module      :  Tools.Metrics.TransporterBPPMetrics.Types

Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Tools.Metrics.TransporterBPPMetrics.Types
  ( HasBPPMetrics,
    BPPMetricsContainer (..),
    module CoreMetrics,
    registerTransporterMetricsContainer,
    TransporterMetricsContainer (..),
    HasTransporterMetrics,
    registerBPPMetricsContainer,
  )
where

import Beckn.Tools.Metrics.CoreMetrics as CoreMetrics
import EulerHS.Prelude
import Prometheus as P
import Utils.Common

type HasBPPMetrics m r = (HasFlowEnv m r '["bppMetrics" ::: BPPMetricsContainer])

type SearchDurationMetric = (P.Vector P.Label1 P.Histogram, P.Vector P.Label1 P.Counter)

data BPPMetricsContainer = BPPMetricsContainer
  { searchDurationTimeout :: Seconds,
    searchDuration :: SearchDurationMetric
  }

type HasTransporterMetrics m r = HasFlowEnv m r '["transporterMetrics" ::: TransporterMetricsContainer]

data TransporterMetricsContainer = TransporterMetricsContainer
  { realFareDeviation :: P.Histogram,
    realDistanceDeviation :: P.Histogram
  }

registerTransporterMetricsContainer :: IO TransporterMetricsContainer
registerTransporterMetricsContainer =
  TransporterMetricsContainer
    <$> (P.register . P.histogram fareDeviation $ aroundZero 10 5)
    <*> (P.register . P.histogram distanceDeviation $ aroundZero 10 6)
  where
    aroundZero factor b =
      let l = P.exponentialBuckets 1 factor b
       in reverse (map negate l) ++ l
    fareDeviation =
      P.Info
        "BPP_fare_deviation"
        "Difference between initially offered and recalculated fare of a ride"
    distanceDeviation =
      P.Info
        "BPP_distance_deviation"
        "Difference between estimated distance and real distance of a ride"

registerBPPMetricsContainer :: Seconds -> IO BPPMetricsContainer
registerBPPMetricsContainer searchDurationTimeout = do
  searchDuration <- registerSearchDurationMetric searchDurationTimeout
  return $ BPPMetricsContainer {..}

registerSearchDurationMetric :: Seconds -> IO SearchDurationMetric
registerSearchDurationMetric searchDurationTimeout = do
  searchDurationHistogram <-
    P.register $
      P.vector "transporter_id" $
        P.histogram
          infoSearchDuration
          buckets
  failureCounter <-
    P.register $
      P.vector "transporter_id" $
        P.counter $ P.Info "BPP_search_failure_counter" ""

  pure (searchDurationHistogram, failureCounter)
  where
    infoSearchDuration =
      P.Info
        "BPP_search_time"
        ""
    buckets =
      P.linearBuckets
        0
        0.5
        searchDurationBucketCount
    searchDurationBucketCount = (getSeconds searchDurationTimeout + 1) * 2
