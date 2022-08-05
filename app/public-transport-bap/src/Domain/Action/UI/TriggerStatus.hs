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

Module      :  Domain.Action.UI.TriggerStatus
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Domain.Action.UI.TriggerStatus where

import Beckn.Prelude
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Domain.Types.Booking.Type as DBooking
import qualified Storage.Queries.Booking as QBooking
import Tools.Error

data StatusRes = StatusRes
  { bookingId :: Id DBooking.Booking,
    ticketId :: Text,
    bppId :: Text,
    bppUrl :: BaseUrl
  }

triggerStatusUpdate :: EsqDBFlow m r => Id DBooking.Booking -> m StatusRes
triggerStatusUpdate bookingId = do
  booking <- QBooking.findById bookingId >>= fromMaybeM (BookingDoesNotExist bookingId.getId)
  ticketId <- booking.ticketId & fromMaybeM BookingBppOrderIdNotFound
  pure
    StatusRes
      { bppId = booking.bppId,
        bppUrl = booking.bppUrl,
        ..
      }
