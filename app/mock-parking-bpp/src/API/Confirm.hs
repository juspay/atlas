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

Module      :  API.Confirm
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module API.Confirm where

import API.Utils (buildOnActionContext)
import Beckn.Mock.App
import Beckn.Mock.Utils
import Beckn.Types.Core.Ack
import Beckn.Types.Core.Context
import Beckn.Types.Core.ReqTypes
import Beckn.Types.Error
import Beckn.Types.Forkable
import Beckn.Utils.Error.Throwing
import Beckn.Utils.Logging
import Beckn.Utils.Time
import Core.Common.Payment
import qualified Core.Confirm as Confirm
import Core.OnConfirm
import qualified Core.OnStatus as OnStatus
import Environment
import ExternalAPI
import MockData.OnConfirm
import qualified Redis
import Relude hiding (state)
import Utils

confirmServer :: BecknReq Confirm.ConfirmMessage -> MockM AppEnv AckResponse
confirmServer confirmReq@(BecknReq ctx req) = do
  logDebug $ "request body: " <> show confirmReq
  context' <- buildOnActionContext ON_CONFIRM ctx
  orderId <- generateOrderId
  let eithOrder = buildOnConfirmOrder orderId req.order
  _ <- fork "call on_confirm" $ do
    waitMilliSec <- asks (.callbackWaitTimeMilliSec)
    threadDelayMilliSec waitMilliSec
    let eithOnConfirmCallbackData = OnConfirmMessage <$> first textToError eithOrder
    ack <- callBapOnConfirm $ BecknCallbackReq context' eithOnConfirmCallbackData
    logDebug $ "got ack" <> show ack
    whenRight_ eithOrder $ \onConfirmOrder -> do
      Redis.writeOrder context' onConfirmOrder
      trackPayment onConfirmOrder.id

  pure Ack

data HandlingWay = Success | FailedPayment | LinkExpired
  deriving (Show)

defineHandlingWay :: Text -> HandlingWay
defineHandlingWay = \case
  "4" -> Success
  "9" -> LinkExpired
  _ -> FailedPayment

trackPayment :: Text -> MockM AppEnv ()
trackPayment orderId = do
  secondsToWait <- asks (.statusWaitTimeSec)
  logInfo $ "waiting " <> show secondsToWait <> " seconds before changing payment status"
  threadDelaySec secondsToWait
  (context, order) <- Redis.readOrder orderId
  item <- fromEitherM InvalidRequest $ validateUnique "item" order.items
  let handlingWay = defineHandlingWay item.id
  logInfo $ "handling item with id = " <> item.id <> " using a handling way: " <> show handlingWay
  case handlingWay of
    Success -> transactionOk context order
    FailedPayment -> cancelTransaction REFUNDED context order -- where is FAILED constructor?
    LinkExpired -> cancelTransaction PAYMENT_LINK_EXPIRED context order

transactionOk :: Context -> Order -> MockM AppEnv ()
transactionOk context order = do
  let onStatusMessage = OnStatus.OnStatusMessage $ successfulPayment order
      onStatusReq = BecknCallbackReq (context {action = ON_STATUS}) (Right onStatusMessage)
  _ <- Redis.editOrder successfulPayment order.id
  -- what if we failed to change the state?
  callBapOnStatus onStatusReq

successfulPayment :: Order -> Order
successfulPayment = changePaymentStatus (Just ACTIVE) CAPTURED PAID

cancelTransaction :: PaymentGatewayTransactionStatus -> Context -> Order -> MockM AppEnv ()
cancelTransaction trStatus context order = do
  let onStatusMessage = OnStatus.OnStatusMessage $ paymentFailed trStatus order
      onStatusReq = BecknCallbackReq context {action = ON_STATUS} $ Right onStatusMessage
  _ <- Redis.editOrder (paymentFailed trStatus) order.id
  callBapOnStatus onStatusReq

-- this function should use on_cancel endpoint, but it is currently absent in the parking-bap

paymentFailed :: PaymentGatewayTransactionStatus -> Order -> Order
paymentFailed trStatus = changePaymentStatus (Just CANCELLED) trStatus NOT_PAID

changePaymentStatus :: Maybe OrderState -> PaymentGatewayTransactionStatus -> PaymentStatus -> Order -> Order
changePaymentStatus orderState trStatus paymStatus order =
  order
    { state = orderState,
      payment =
        order.payment
          { params =
              order.payment.params
                { transaction_status = trStatus
                },
            status = paymStatus
          }
    }
