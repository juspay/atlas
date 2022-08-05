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
import Beckn.Storage.Hedis as Hedis
import Beckn.Streaming.Kafka.Consumer
import Beckn.Streaming.Kafka.Topic.PublicTransportQuoteList
import Beckn.Streaming.MonadConsumer
import Beckn.Types.App (MonadFlow)
import Beckn.Types.Logging
import Beckn.Utils.Logging
import Control.Concurrent.STM.TMVar
import GHC.Conc

run ::
  ( HasField "isShuttingDown" r (TMVar ()),
    MonadConsumer PublicTransportQuoteList m,
    HedisFlow m r,
    MonadFlow m
  ) =>
  m ()
run = do
  withLogTag "Service" $ do
    listenForMessages @PublicTransportQuoteList isRunning $ \PublicTransportQuoteList {..} ->
      withTransactionIdLogTag' transactionId $
        Hedis.rPushExp (makeHedisKey transactionId) quoteList expirationTime
  where
    makeHedisKey transactionId = "publicTransportQuoteList:" <> transactionId
    expirationTime = 600
    isRunning = liftIO . atomically . isEmptyTMVar =<< asks (.isShuttingDown)
