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

Module      :  Domain.Action.UI.QuoteConfirm
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Domain.Action.UI.QuoteConfirm where

import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Error
import Beckn.Types.Id
import Beckn.Utils.Common
import Beckn.Utils.GenericPretty
import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.Quote as DQuote
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.Quote as QQuote
import Tools.Auth

data QConfirmReq = QConfirmReq
  { quantity :: Int,
    requestorName :: Text
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema, PrettyShow)

newtype QConfirmRes = QConfirmRes
  { booking_id :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema, PrettyShow)

data ConfirmMessageD = ConfirmMessageD
  { txnId :: Text,
    quantity :: Int,
    requestorName :: Text,
    booking :: DBooking.Booking,
    quote :: DQuote.Quote
  }

validateConfirmReq :: EsqDBFlow m r => QConfirmReq -> m ()
validateConfirmReq confirmReq = do
  when (confirmReq.quantity <= 0) $ throwError $ InvalidRequest "invalid quantity value"

quoteConfirm :: EsqDBFlow m r => PersonId -> Id DQuote.Quote -> QConfirmReq -> m (QConfirmRes, ConfirmMessageD)
quoteConfirm personId quoteId confirmReq = do
  validateConfirmReq confirmReq
  quote <- QQuote.findById quoteId >>= fromMaybeM (QuoteNotFound quoteId.getId)
  bookingId <- generateGUID
  let txnId = bookingId
  now <- getCurrentTime
  let booking = buildBooking now bookingId personId confirmReq quote
  _ <- Esq.runTransaction $ QBooking.create booking
  pure (QConfirmRes bookingId, makeConfirmMessageD txnId confirmReq quote booking)

makeConfirmMessageD :: Text -> QConfirmReq -> DQuote.Quote -> DBooking.Booking -> ConfirmMessageD
makeConfirmMessageD txnId qConfirmReq quote booking = do
  let quantity = qConfirmReq.quantity
      requestorName = qConfirmReq.requestorName
  ConfirmMessageD {..}

buildBooking :: UTCTime -> Text -> PersonId -> QConfirmReq -> DQuote.Quote -> DBooking.Booking
buildBooking now bookingId personId confirmReq quote = do
  let id = Id bookingId
      searchId = quote.searchId
      quoteId = quote.id
      bknTxnId = bookingId
      requestorId = personId
      quantity = confirmReq.quantity
      bppId = quote.bppId
      bppUrl = quote.bppUrl
      publicTransportSupportNumber = "support number" -- FIXME
      description = quote.description
      fare = quote.fare -- fare of one ticket
      departureTime = quote.departureTime
      arrivalTime = quote.arrivalTime
      departureStationId = quote.departureStationId
      arrivalStationId = quote.arrivalStationId
      status = DBooking.NEW
      createdAt = now
      updatedAt = now
      ticketId = Nothing
      ticketCreatedAt = Nothing
  DBooking.Booking {..}
