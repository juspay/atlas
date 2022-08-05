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

Module      :  API.Parking.Booking.BookingId.TriggerStatus.Handler
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module API.Parking.Booking.BookingId.TriggerStatus.Handler where

import API.Parking.Booking.BookingId.TriggerStatus.Types
import App.Types
import Beckn.Prelude
import Beckn.Types.APISuccess
import Beckn.Types.Core.Context
import Beckn.Types.Core.ReqTypes
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Core.Status as Status
import qualified Domain.Booking.Type as DBooking (Booking)
import qualified ExternalAPI.Flow as ExternalAPI
import qualified Storage.Queries.Booking as QBooking
import Tools.Auth
import Tools.Error

handler :: FlowServer API
handler = triggerStatusUpdate

triggerStatusUpdate :: PersonId -> Id DBooking.Booking -> FlowHandler APISuccess
triggerStatusUpdate _ bookingId = withFlowHandlerAPI $ do
  booking <- QBooking.findById bookingId >>= fromMaybeM (BookingDoesNotExist bookingId.getId)
  bppOrderId <- booking.bppOrderId & fromMaybeM BookingBppOrderIdNotFound
  let url = booking.bppUrl
  context <- buildParkingContext STATUS (getId bookingId)
  ExternalAPI.triggerStatusUpdate url (BecknReq context (makeStatusMessage bppOrderId))
  pure Success
  where
    buildParkingContext action txnId = do
      currTime <- getCurrentTime
      msgId <- generateGUIDText
      bapId <- asks (.selfId)
      bapUri <- asks (.selfURI)
      return $
        Context
          { domain = PARKING,
            country = "IND",
            city = "Kochi",
            action = action,
            core_version = "0.9.0",
            bap_id = bapId,
            bap_uri = bapUri,
            bpp_id = Nothing,
            bpp_uri = Nothing,
            transaction_id = Just txnId,
            message_id = msgId,
            timestamp = currTime
          }
    makeStatusMessage bppOrderId =
      Status.StatusMessage
        { order =
            Status.Order
              { id = bppOrderId
              }
        }
