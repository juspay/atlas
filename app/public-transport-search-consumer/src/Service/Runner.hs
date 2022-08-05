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

Module      :  Service.Runner
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Service.Runner where

import Beckn.Prelude
import Beckn.Streaming.Kafka.Consumer
import Beckn.Streaming.Kafka.Topic.PublicTransportSearch
import Beckn.Streaming.MonadConsumer
import Beckn.Types.Logging
import Beckn.Utils.Common
import Control.Concurrent.STM.TMVar
import qualified Core.ACL.Search as BecknACL
import qualified Domain.Action.Search as DSearch
import qualified ExternalAPI.Flow as ExternalAPI
import GHC.Conc
import Tools.Metrics

run ::
  ( HasField "isShuttingDown" r (TMVar ()),
    HasField "bapId" r Text,
    HasField "bapURI" r BaseUrl,
    HasField "gatewayUrl" r BaseUrl,
    CoreMetrics m,
    HasHttpClientOptions r c,
    MonadConsumer PublicTransportSearch m,
    EsqDBFlow m r,
    MonadFlow m
  ) =>
  m ()
run = withLogTag "Service" $
  listenForMessages @PublicTransportSearch isRunning $ \searchReq -> withTransactionIdLogTag' searchReq.id $ do
    searchMessage <- DSearch.search searchReq
    becknSearchReq <- BecknACL.buildSearchReq searchMessage
    fork "search" . withRetry $ do
      -- do we need fork here?
      ExternalAPI.search becknSearchReq
  where
    isRunning = liftIO . atomically . isEmptyTMVar =<< asks (.isShuttingDown)
