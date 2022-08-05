{-# LANGUAGE TypeApplications #-}


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

Module      :  API.Status
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module API.Status where

import API.Utils (buildOnActionContext)
import Beckn.Mock.App
import Beckn.Mock.Utils
import Beckn.Types.Core.Ack
import Beckn.Types.Core.Context
import Beckn.Types.Core.ReqTypes
import Beckn.Types.Forkable
import Beckn.Utils.Logging
import Beckn.Utils.Time
import qualified Control.Monad.Catch as C
import Core.OnStatus
import Core.Status
import Environment
import ExternalAPI
import qualified Redis
import Relude hiding (state)

statusServer :: BecknReq StatusMessage -> MockM AppEnv AckResponse
statusServer (BecknReq ctx msg) = do
  logInfo $ "got confirm request: " <> show msg
  context' <- buildOnActionContext ON_STATUS ctx
  let orderId = msg.order.id
  eithCtxOrd <- C.try @(MockM AppEnv) @SomeException (Redis.readOrder orderId)
  _ <- fork "call on_status" $ do
    waitMilliSec <- asks (.callbackWaitTimeMilliSec)
    threadDelayMilliSec waitMilliSec
    let eithOnStatusMsg = bimap (textToError . show) (OnStatusMessage . snd) eithCtxOrd
        onStatusReq = BecknCallbackReq context' eithOnStatusMsg
    callBapOnStatus onStatusReq
  pure Ack
