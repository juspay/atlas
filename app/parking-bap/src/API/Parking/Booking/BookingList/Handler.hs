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

Module      :  API.Parking.Booking.BookingList.Handler
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module API.Parking.Booking.BookingList.Handler where

import API.Parking.Booking.BookingList.Types
import App.Types
import Beckn.Prelude
import Beckn.Types.Id
import Beckn.Utils.Common
import Domain.Booking.API as Booking
import Domain.Booking.Type
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.ParkingLocation as QPLocation
import qualified Storage.Queries.PaymentTransaction as QPT
import Tools.Auth
import Tools.Error

handler :: FlowServer API
handler = bookingList

bookingList :: PersonId -> Maybe Integer -> Maybe Integer -> FlowHandler Booking.BookingListRes
bookingList personId mbLimit mbOffset = withFlowHandlerAPI $ do
  let limit = fromMaybe 10 mbLimit
      offset = fromMaybe 0 mbOffset
  bList <- QBooking.findAllByRequestorId personId limit offset
  Booking.BookingListRes <$> traverse buildBookingListRes bList

buildBookingListRes :: EsqDBFlow m r => Booking -> m Booking.BookingAPIEntity
buildBookingListRes booking = do
  location <- QPLocation.findById (Id booking.parkingSpaceLocationId) >>= fromMaybeM (ParkingLocationNotFound booking.parkingSpaceLocationId)
  paymentTrans <- QPT.findByBookingId booking.id
  return $ Booking.makeBookingAPIEntity booking location paymentTrans
