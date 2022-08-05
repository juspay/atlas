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

Module      :  API.Beckn.OnConfirm.Handler
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module API.Beckn.OnConfirm.Handler where

import qualified API.Beckn.OnConfirm.Types as OnConfirm
import App.Types
import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Core.ReqTypes
import Beckn.Types.Id
import Beckn.Utils.Common
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult)
import qualified Core.Common.Context as Context
import qualified Core.OnConfirm as OnConfirm
import Domain.Booking as DBooking
import Domain.PaymentTransaction (PaymentStatus (PENDING))
import qualified Domain.PaymentTransaction as DPaymentTransaction
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.PaymentTransaction as PaymentTransactionDB
import Tools.Context (validateContext)
import Tools.Error

handler ::
  SignatureAuthResult ->
  FlowServer OnConfirm.API
handler = onConfirm

onConfirm ::
  SignatureAuthResult ->
  BecknCallbackReq OnConfirm.OnConfirmMessage ->
  FlowHandler AckResponse
onConfirm _ req = withFlowHandlerBecknAPI . withTransactionIdLogTag req $ do
  logTagDebug "on_confirm req" (encodeToText req)
  validateContext Context.ON_CONFIRM $ req.context
  transactionId <- req.context.transaction_id & fromMaybeM (InvalidRequest "Context.transaction_id is not present.")
  case req.contents of
    Right msg -> handleOnConfirm (Id transactionId) msg
    Left err -> logTagError "on_confirm req" $ "on_confirm error: " <> show err
  return Ack

handleOnConfirm :: EsqDBFlow m r => Id DBooking.Booking -> OnConfirm.OnConfirmMessage -> m ()
handleOnConfirm bookingId msg = do
  booking <- QBooking.findById bookingId >>= fromMaybeM (BookingDoesNotExist bookingId.getId)
  now <- getCurrentTime
  let updBooking =
        booking
          { status = DBooking.AWAITING_PAYMENT,
            bppOrderId = Just msg.order.id,
            ticketId = Just msg.order.id,
            ticketCreatedAt = Just now
          }
  paymentData <- buildPaymentData updBooking msg
  runTransaction $ do
    QBooking.updateStatusAndBppOrderId updBooking
    PaymentTransactionDB.create paymentData

buildPaymentData :: MonadFlow m => DBooking.Booking -> OnConfirm.OnConfirmMessage -> m DPaymentTransaction.PaymentTransaction
buildPaymentData booking msg = do
  id <- generateGUID
  now <- getCurrentTime
  return
    DPaymentTransaction.PaymentTransaction
      { id = Id id,
        bookingId = booking.id,
        paymentGatewayTxnId = msg.order.payment.params.transaction_id,
        paymentGatewayTxnStatus = show msg.order.payment.params.transaction_status,
        fare = booking.fare,
        status = PENDING,
        paymentUrl = msg.order.payment.uri,
        updatedAt = now,
        createdAt = now
      }
