{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}


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

Module      :  Storage.Tabular.RideBooking
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Storage.Tabular.RideBooking where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Amount
import Beckn.Types.Id
import qualified Domain.Types.RideBooking as Domain
import qualified Storage.Tabular.Person as SPerson
import qualified Storage.Tabular.Quote as SQuote
import qualified Storage.Tabular.SearchReqLocation as SLoc
import qualified Storage.Tabular.SearchRequest as SSearchRequest

derivePersistField "Domain.RideBookingStatus"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    RideBookingT sql=ride_booking
      id Text
      bppBookingId Text Maybe sql=bpp_ride_booking_id
      requestId SSearchRequest.SearchRequestTId
      quoteId SQuote.QuoteTId
      status Domain.RideBookingStatus
      providerId Text
      providerUrl Text
      providerName Text
      providerMobileNumber Text
      startTime UTCTime
      riderId SPerson.PersonTId
      fromLocationId SLoc.SearchReqLocationTId
      toLocationId SLoc.SearchReqLocationTId Maybe
      estimatedFare Amount
      discount Amount Maybe
      estimatedTotalFare Amount
      distance Double Maybe
      vehicleVariant Text
      createdAt UTCTime
      updatedAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey RideBookingT where
  type DomainKey RideBookingT = Id Domain.RideBooking
  fromKey (RideBookingTKey _id) = Id _id
  toKey (Id id) = RideBookingTKey id

instance TEntity RideBookingT Domain.RideBooking where
  fromTEntity entity = do
    let RideBookingT {..} = entityVal entity
    pUrl <- parseBaseUrl providerUrl
    return $
      Domain.RideBooking
        { id = Id id,
          bppBookingId = Id <$> bppBookingId,
          requestId = fromKey requestId,
          quoteId = fromKey quoteId,
          riderId = fromKey riderId,
          fromLocationId = fromKey fromLocationId,
          toLocationId = fromKey <$> toLocationId,
          providerUrl = pUrl,
          ..
        }
  toTType Domain.RideBooking {..} =
    RideBookingT
      { id = getId id,
        bppBookingId = getId <$> bppBookingId,
        requestId = toKey requestId,
        quoteId = toKey quoteId,
        riderId = toKey riderId,
        fromLocationId = toKey fromLocationId,
        toLocationId = toKey <$> toLocationId,
        providerUrl = showBaseUrl providerUrl,
        ..
      }
  toTEntity a =
    Entity (toKey a.id) $ toTType a
