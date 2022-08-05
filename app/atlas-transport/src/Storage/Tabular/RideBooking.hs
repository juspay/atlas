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
import qualified Domain.Types.Vehicle as Veh
import qualified Storage.Queries.RideBooking.RentalRideBooking as QRentalRideBooking
import Storage.Tabular.FareProduct ()
import Storage.Tabular.Organization (OrganizationTId)
import Storage.Tabular.Quote (QuoteTId)
import Storage.Tabular.RiderDetails (RiderDetailsTId)
import Storage.Tabular.SearchReqLocation (SearchReqLocationTId)
import Storage.Tabular.SearchRequest (SearchRequestTId)
import Storage.Tabular.Vehicle ()
import Types.Error
import Utils.Common hiding (id)

derivePersistField "Domain.RideBookingStatus"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    RideBookingT sql=ride_booking
      id Text
      messageId Text
      requestId SearchRequestTId
      quoteId QuoteTId
      status Domain.RideBookingStatus
      providerId OrganizationTId
      bapId Text
      bapUri Text
      startTime UTCTime
      riderId RiderDetailsTId
      fromLocationId SearchReqLocationTId
      toLocationId SearchReqLocationTId Maybe
      vehicleVariant Veh.Variant
      estimatedFare Amount
      discount Amount Maybe
      estimatedTotalFare Amount
      estimatedDistance Double Maybe
      reallocationsCount Int
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
    pUrl <- parseBaseUrl bapUri
    rideBookingDetails <- case fromKey <$> toLocationId of
      Just toLocationId' -> do
        estimatedDistance' <-
          estimatedDistance & fromMaybeM (InternalError "Missing estimatedDistance for one way ride booking")
        pure $
          Domain.OneWayDetails
            Domain.OneWayRideBookingDetails
              { estimatedDistance = estimatedDistance',
                toLocationId = toLocationId'
              }
      Nothing -> do
        rentalRideBooking <- QRentalRideBooking.findByRideBookingId (Id id) >>= fromMaybeM (RideBookingDoesNotExist id)
        return $ Domain.mkRentalRideBookingDetails rentalRideBooking.rentalFarePolicyId

    return $
      Domain.RideBooking
        { id = Id id,
          requestId = fromKey requestId,
          quoteId = fromKey quoteId,
          riderId = fromKey riderId,
          fromLocationId = fromKey fromLocationId,
          providerId = fromKey providerId,
          bapUri = pUrl,
          ..
        }
  toTType Domain.RideBooking {..} = do
    let (toLocationId, estimatedDistance) = case rideBookingDetails of
          Domain.OneWayDetails details -> (Just details.toLocationId, Just details.estimatedDistance)
          Domain.RentalDetails _ -> (Nothing, Nothing)
    RideBookingT
      { id = getId id,
        requestId = toKey requestId,
        quoteId = toKey quoteId,
        riderId = toKey riderId,
        fromLocationId = toKey fromLocationId,
        toLocationId = toKey <$> toLocationId,
        providerId = toKey providerId,
        bapUri = showBaseUrl bapUri,
        ..
      }
  toTEntity a =
    Entity (toKey a.id) $ toTType a
