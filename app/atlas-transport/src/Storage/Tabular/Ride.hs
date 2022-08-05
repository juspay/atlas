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

Module      :  Storage.Tabular.Ride
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Storage.Tabular.Ride where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Amount (Amount)
import Beckn.Types.Id
import qualified Domain.Types.Ride as Domain
import Storage.Tabular.Person (PersonTId)
import Storage.Tabular.RideBooking (RideBookingTId)
import Storage.Tabular.Vehicle (VehicleTId)

derivePersistField "Domain.RideStatus"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    RideT sql=ride
      id Text
      bookingId RideBookingTId
      shortId Text
      status Domain.RideStatus
      driverId PersonTId
      vehicleId VehicleTId
      otp Text
      trackingUrl Text
      fare Amount Maybe
      totalFare Amount Maybe
      traveledDistance Double
      chargeableDistance Double Maybe
      tripStartTime UTCTime Maybe
      tripEndTime UTCTime Maybe
      createdAt UTCTime
      updatedAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey RideT where
  type DomainKey RideT = Id Domain.Ride
  fromKey (RideTKey _id) = Id _id
  toKey (Id id) = RideTKey id

instance TEntity RideT Domain.Ride where
  fromTEntity entity = do
    let RideT {..} = entityVal entity
    return $
      Domain.Ride
        { id = Id id,
          bookingId = fromKey bookingId,
          shortId = ShortId shortId,
          driverId = fromKey driverId,
          vehicleId = fromKey vehicleId,
          ..
        }
  toTType Domain.Ride {..} =
    RideT
      { id = getId id,
        bookingId = toKey bookingId,
        shortId = getShortId shortId,
        driverId = toKey driverId,
        vehicleId = toKey vehicleId,
        ..
      }
  toTEntity a =
    Entity (toKey a.id) $ toTType a
