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

Module      :  API.Parking.Quotes.QuoteId.Confirm.Handler
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module API.Parking.Quotes.QuoteId.Confirm.Handler where

import API.Parking.Quotes.QuoteId.Confirm.Types
import App.Types
import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Core.ReqTypes
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Core.Common.Billing as Billing
import qualified Core.Common.Context as Context
import qualified Core.Common.Time as Time
import qualified Core.Common.Vehicle as Vehicle
import qualified Core.Confirm as Confirm
import qualified Domain.Booking as DBooking
import qualified Domain.Quote as DQuote
import qualified ExternalAPI.Flow as ExternalAPI
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.Search as QSearch
import Tools.Auth
import Tools.Context (buildContext)
import Tools.Error

handler :: Id DQuote.Quote -> FlowServer API
handler = confirm

confirm :: Id DQuote.Quote -> PersonId -> PostQuoteConfirmReq -> FlowHandler PostQuoteConfirmRes
confirm quoteId _ req = withFlowHandlerAPI $ do
  quote <- QQuote.findById quoteId >>= fromMaybeM (QuoteDoesNotExist quoteId.getId)
  search <- QSearch.findById quote.searchId >>= fromMaybeM (SearchNotFound quote.searchId.getId)
  booking <- buildBooking search quote
  _ <- Esq.runTransaction $ QBooking.create booking
  let txnId = booking.id.getId
  bapURI <- asks (.selfURI)
  context <- buildContext Context.CONFIRM txnId bapURI (Just booking.bppUrl)
  ExternalAPI.confirm booking.bppUrl (BecknReq context $ mkConfirmMessage booking)
  return $ PostQuoteConfirmRes booking.id
  where
    buildBooking search quote = do
      uid <- generateGUID
      now <- getCurrentTime
      return $
        DBooking.Booking
          { id = Id uid,
            searchId = search.id,
            quoteId = quote.id,
            requestorId = search.requestorId,
            requestorNumber = req.requestorNumber,
            vehicleNumber = req.vehicleNumber,
            bppId = quote.bppId,
            bppUrl = quote.bppUrl,
            bppItemId = quote.bppItemId,
            parkingSpaceName = quote.parkingSpaceName,
            parkingSpaceLocationId = getId quote.parkingLocationId,
            fare = quote.fare,
            fromDate = search.fromDate,
            toDate = search.toDate,
            status = DBooking.NEW,
            ticketId = Nothing,
            ticketCreatedAt = Nothing,
            updatedAt = now,
            createdAt = now,
            bppOrderId = Nothing,
            requestorName = req.requestorName
          }
    mkConfirmMessage booking = do
      let vehicle = Vehicle.Vehicle booking.vehicleNumber
          end = Confirm.StartEnd $ Time.Time booking.toDate
          start = Confirm.StartEnd $ Time.Time booking.fromDate
          fulfillment = Confirm.Fulfillment {..}
          billing =
            Billing.Billing
              { phone = booking.requestorNumber,
                name = booking.requestorName
              }
          items = [Confirm.Item booking.bppItemId $ Confirm.Quantity 1]
          locations = [Confirm.Location booking.parkingSpaceLocationId]
          provider = Confirm.Provider {..}
          order = Confirm.Order {..}
      Confirm.ConfirmMessage {..}
