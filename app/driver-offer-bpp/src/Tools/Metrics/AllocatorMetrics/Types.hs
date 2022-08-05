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

Module      :  Tools.Metrics.AllocatorMetrics.Types

Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Tools.Metrics.AllocatorMetrics.Types
  ( HasAllocatorMetrics,
    AllocatorMetricsContainer (..),
    module CoreMetrics,
    registerAllocatorMetricsContainer,
  )
where

import Beckn.Tools.Metrics.CoreMetrics as CoreMetrics
import EulerHS.Prelude
import Prometheus as P
import Utils.Common

type HasAllocatorMetrics m r = (HasFlowEnv m r '["btmMetrics" ::: AllocatorMetricsContainer])

type TaskCounterMetric = P.Counter

type TaskDurationMetric = P.Histogram

type FailedTaskCounterMetric = P.Counter

data AllocatorMetricsContainer = AllocatorMetricsContainer
  { taskCounter :: TaskCounterMetric,
    taskDuration :: TaskDurationMetric,
    failedTaskCounter :: FailedTaskCounterMetric
  }

registerAllocatorMetricsContainer :: IO AllocatorMetricsContainer
registerAllocatorMetricsContainer = do
  taskCounter <- registerTaskCounter
  taskDuration <- registerTaskDurationMetric
  failedTaskCounter <- registerFailedTaskCounter
  return $ AllocatorMetricsContainer {..}

registerTaskCounter :: IO TaskCounterMetric
registerTaskCounter = P.register . P.counter $ P.Info "BTM_task_count" ""

registerFailedTaskCounter :: IO FailedTaskCounterMetric
registerFailedTaskCounter = P.register . P.counter $ P.Info "BTM_failed_task_count" ""

registerTaskDurationMetric :: IO TaskDurationMetric
registerTaskDurationMetric = P.register . P.histogram (P.Info "BTM_task_duration" "") $ P.linearBuckets 0 0.1 20
