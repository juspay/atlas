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

Module      :  API.Beckn.OnStatus.Handler
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module API.Beckn.OnStatus.Handler where

import qualified API.Beckn.OnStatus.Types as OnStatus
import App.Types
import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Core.Ack
import Beckn.Types.Core.ReqTypes
import Beckn.Utils.Common
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult)
import qualified Core.Common.Context as Context
import qualified Core.Common.Payment as Payment
import qualified Core.OnStatus as OnStatus
import Core.OnStatus.Order (OrderState (ACTIVE, CANCELLED, COMPLETE))
import qualified Domain.Booking as BookingStatus
import qualified Domain.PaymentTransaction as PaymentStatus
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.PaymentTransaction as PaymentTransactionDB
import Tools.Context (validateContext)
import Tools.Error

handler ::
  SignatureAuthResult ->
  FlowServer OnStatus.API
handler = onStatus

onStatus ::
  SignatureAuthResult ->
  BecknCallbackReq OnStatus.OnStatusMessage ->
  FlowHandler AckResponse
onStatus _ req = withFlowHandlerBecknAPI . withTransactionIdLogTag req $ do
  logTagDebug "on_status req" (encodeToText req)
  validateContext Context.ON_STATUS $ req.context
  case req.contents of
    Right msg -> handleOnStatus msg
    Left err -> logTagError "on_status req" $ "on_status error: " <> show err
  return Ack

handleOnStatus :: EsqDBFlow m r => OnStatus.OnStatusMessage -> m ()
handleOnStatus msg = do
  let payment = msg.order.payment
      orderId = msg.order.id
      bppOrderStatus = msg.order.state
      bppPaymentStatus = payment.status
      bppPaymentGatewayTxnStatus = payment.params.transaction_status
  booking <- QBooking.findByBppOrderId orderId >>= fromMaybeM (BookingNotFound orderId)
  paymentDetails <- PaymentTransactionDB.findByBookingId booking.id >>= fromMaybeM (PaymentDetailsNotFound booking.id.getId)
  runTransaction $ do
    QBooking.updateStatus booking $ convertBookingStatus bppOrderStatus
    PaymentTransactionDB.updateTxnDetails paymentDetails.id (show bppPaymentGatewayTxnStatus) $ convertPaymentStatus (bppPaymentStatus, bppPaymentGatewayTxnStatus)
  where
    convertBookingStatus = \case
      Just ACTIVE -> BookingStatus.CONFIRMED
      Just COMPLETE -> BookingStatus.CONFIRMED
      Just CANCELLED -> BookingStatus.CANCELLED
      Nothing -> BookingStatus.AWAITING_PAYMENT
    convertPaymentStatus = \case
      (Payment.NOT_PAID, Payment.PAYMENT_LINK_CREATED) -> PaymentStatus.PENDING
      (Payment.NOT_PAID, Payment.PAYMENT_LINK_EXPIRED) -> PaymentStatus.FAILED
      (Payment.NOT_PAID, _) -> PaymentStatus.PENDING
      (Payment.PAID, Payment.CAPTURED) -> PaymentStatus.SUCCESS
      (Payment.PAID, Payment.REFUNDED) -> PaymentStatus.FAILED
      (Payment.PAID, _) -> PaymentStatus.PENDING
