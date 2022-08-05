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

Module      :  Domain.Action.Beckn.OnStatus
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Domain.Action.Beckn.OnStatus where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import Beckn.Utils.Common
import Beckn.Utils.GenericPretty (PrettyShow)
import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.PaymentTransaction as DPaymentTransaction
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.PaymentTransaction as QPaymentTransaction
import Tools.Error

data OnStatusReq = OnStatusReq
  { bookingId :: Id DBooking.Booking,
    bookingStatus :: DBooking.BookingStatus,
    transactionStatus :: Text,
    paymentStatus :: DPaymentTransaction.PaymentStatus
  }
  deriving (Show, Generic, PrettyShow)

handler :: EsqDBFlow m r => OnStatusReq -> m ()
handler OnStatusReq {..} = do
  booking <- QBooking.findById bookingId >>= fromMaybeM (BookingNotFound bookingId.getId)
  paymentDetails <- QPaymentTransaction.findByBookingId booking.id >>= fromMaybeM (PaymentDetailsNotFound booking.id.getId)
  runTransaction $ do
    QBooking.updateStatus booking bookingStatus
    QPaymentTransaction.updateTxnDetails paymentDetails.id transactionStatus paymentStatus
