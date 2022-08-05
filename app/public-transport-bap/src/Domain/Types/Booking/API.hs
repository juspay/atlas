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

Module      :  Domain.Types.Booking.API
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Domain.Types.Booking.API where

import Beckn.Prelude
import Beckn.Types.Amount
import Beckn.Types.Id
import Domain.Types.Booking.Type
import Domain.Types.PaymentTransaction
import Domain.Types.TransportStation

data BookingAPIEntity = BookingAPIEntity
  { id :: Id Booking,
    quantity :: Int,
    publicTransportSupportNumber :: Text,
    description :: Text,
    fare :: Amount,
    departureTime :: UTCTime,
    arrivalTime :: UTCTime,
    departureStation :: TransportStationAPIEntity,
    arrivalStation :: TransportStationAPIEntity,
    status :: BookingStatus,
    paymentTxn :: Maybe PaymentTransactionAPIEntity,
    ticketId :: Maybe Text,
    ticketCreatedAt :: Maybe UTCTime,
    updatedAt :: UTCTime,
    createdAt :: UTCTime
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

makeBookingAPIEntity :: Booking -> TransportStation -> TransportStation -> Maybe PaymentTransaction -> BookingAPIEntity
makeBookingAPIEntity Booking {..} departureStation arrivalStation mbPaymentTxn =
  BookingAPIEntity
    { departureStation = makeTransportStationAPIEntity departureStation,
      arrivalStation = makeTransportStationAPIEntity arrivalStation,
      paymentTxn = makePaymentTransactionAPIEntity <$> mbPaymentTxn,
      ..
    }

newtype BookingListRes = BookingListRes
  { list :: [BookingAPIEntity]
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
