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

Module      :  Domain.Action.Beckn.OnConfirm
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Domain.Action.Beckn.OnConfirm where

import Beckn.Prelude
import Beckn.Storage.Esqueleto (runTransaction)
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Domain.Types.Booking as Domain
import qualified Domain.Types.PaymentTransaction as Domain
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.PaymentTransaction as PaymentTransactionDB
import Tools.Error

data OnConfirmMessageD = OnConfirmMessageD
  { bookingId :: Id Domain.Booking,
    ticketId :: Text,
    paymentGatewayTxnId :: Text,
    paymentGatewayTxnStatus :: Text,
    bookingStatus :: Domain.BookingStatus,
    paymentStatus :: Domain.PaymentStatus,
    paymentUrl :: BaseUrl
  }

handleOnConfirm :: EsqDBFlow m r => OnConfirmMessageD -> m ()
handleOnConfirm msg = do
  booking <- QBooking.findById msg.bookingId >>= fromMaybeM (BookingDoesNotExist msg.bookingId.getId)
  now <- getCurrentTime
  let updBooking =
        booking{status = msg.bookingStatus,
                ticketId = Just msg.ticketId,
                ticketCreatedAt = Just now
               }
  paymentData <- buildPaymentData updBooking msg
  runTransaction $ do
    QBooking.updateStatusAndBppOrderId updBooking
    PaymentTransactionDB.create paymentData

buildPaymentData :: MonadFlow m => Domain.Booking -> OnConfirmMessageD -> m Domain.PaymentTransaction
buildPaymentData booking msg = do
  id <- generateGUID
  now <- getCurrentTime
  return
    Domain.PaymentTransaction
      { id = Id id,
        bookingId = booking.id,
        bknTxnId = booking.bknTxnId,
        paymentGatewayTxnId = msg.paymentGatewayTxnId,
        paymentGatewayTxnStatus = msg.paymentGatewayTxnStatus,
        fare = booking.fare,
        status = msg.paymentStatus,
        paymentUrl = msg.paymentUrl,
        updatedAt = now,
        createdAt = now
      }
