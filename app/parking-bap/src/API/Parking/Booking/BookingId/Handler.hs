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

Module      :  API.Parking.Booking.BookingId.Handler
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module API.Parking.Booking.BookingId.Handler where

import API.Parking.Booking.BookingId.Types
import App.Types
import Beckn.Prelude
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Domain.Booking as DBooking
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.ParkingLocation as QPLocation
import qualified Storage.Queries.PaymentTransaction as QPT
import Tools.Auth
import Tools.Error

handler :: FlowServer API
handler = status

status :: PersonId -> Id DBooking.Booking -> FlowHandler DBooking.BookingAPIEntity
status _ bookingId = withFlowHandlerAPI $ do
  booking <- QBooking.findById bookingId >>= fromMaybeM (BookingDoesNotExist bookingId.getId)
  location <- QPLocation.findById (Id booking.parkingSpaceLocationId) >>= fromMaybeM (ParkingLocationNotFound booking.parkingSpaceLocationId)
  paymentTrans <- QPT.findByBookingId bookingId
  return $ DBooking.makeBookingAPIEntity booking location paymentTrans
