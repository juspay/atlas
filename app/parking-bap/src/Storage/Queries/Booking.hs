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

Module      :  Storage.Queries.Booking
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Storage.Queries.Booking where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Common
import Beckn.Types.Id
import Domain.Booking
import Domain.Quote
import Storage.Tabular.Booking
import Tools.Auth

findById :: Transactionable m => Id Booking -> m (Maybe Booking)
findById = Esq.findById

findByQuoteId :: Transactionable m => Id Quote -> m (Maybe Booking)
findByQuoteId quoteId =
  Esq.findOne $ do
    booking <- from $ table @BookingT
    where_ $ booking ^. BookingQuoteId ==. val (toKey quoteId)
    return booking

create :: Booking -> SqlDB ()
create = create'

update :: Booking -> SqlDB ()
update parkingBooking = do
  now <- getCurrentTime
  update' $ \tbl -> do
    set
      tbl
      [ BookingStatus =. val parkingBooking.status,
        BookingTicketId =. val parkingBooking.ticketId,
        BookingTicketCreatedAt =. val parkingBooking.ticketCreatedAt,
        BookingUpdatedAt =. val now
      ]
    where_ $ tbl ^. BookingId ==. val (getId parkingBooking.id)

updateStatusAndBppOrderId :: Booking -> SqlDB ()
updateStatusAndBppOrderId parkingBooking = do
  now <- getCurrentTime
  update' $ \tbl -> do
    set
      tbl
      [ BookingStatus =. val parkingBooking.status,
        BookingTicketId =. val parkingBooking.ticketId,
        BookingTicketCreatedAt =. val parkingBooking.ticketCreatedAt,
        BookingUpdatedAt =. val now,
        BookingBppOrderId =. val parkingBooking.bppOrderId
      ]
    where_ $ tbl ^. BookingId ==. val (getId parkingBooking.id)

updateStatus :: Booking -> BookingStatus -> SqlDB ()
updateStatus booking newStatus = do
  now <- getCurrentTime
  update' $ \tbl -> do
    set
      tbl
      [ BookingStatus =. val newStatus,
        BookingUpdatedAt =. val now
      ]
    where_ $ tbl ^. BookingId ==. val (getId booking.id)

findByBppOrderId :: Transactionable m => Text -> m (Maybe Booking)
findByBppOrderId bppOrderId =
  Esq.findOne $ do
    parkingSearch <- from $ table @BookingT
    where_ $ parkingSearch ^. BookingBppOrderId ==. val (Just bppOrderId)
    return parkingSearch

findAllByRequestorId :: Transactionable m => PersonId -> Integer -> Integer -> m [Booking]
findAllByRequestorId personId limitInt offSetInt = do
  let limit_ :: Int64 = fromInteger limitInt
      offset_ :: Int64 = fromInteger offSetInt
  Esq.findAll $ do
    parkingSearch <- from $ table @BookingT
    where_ $ parkingSearch ^. BookingRequestorId ==. val (getId personId)
    limit limit_
    offset offset_
    return parkingSearch
