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

Module      :  Storage.Tabular.RideBooking.RentalRideBooking
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Storage.Tabular.RideBooking.RentalRideBooking where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import qualified Domain.Types.RideBooking as Domain
import qualified Domain.Types.RideBooking.RentalRideBooking as Domain
import Storage.Tabular.RentalFarePolicy (RentalFarePolicyTId)

-- FIXME rideBookingId should be RideBookingTId, but I made it Text to avoid cyclic dependencies
mkPersist
  defaultSqlSettings
  [defaultQQ|
    RentalRideBookingT sql=rental_ride_booking
      rideBookingId Text
      rentalFarePolicyId RentalFarePolicyTId
      Primary rideBookingId
      deriving Generic
    |]

instance TEntityKey RentalRideBookingT where
  type DomainKey RentalRideBookingT = Id Domain.RideBooking
  fromKey (RentalRideBookingTKey _id) = Id _id
  toKey (Id id) = RentalRideBookingTKey id

instance TEntity RentalRideBookingT Domain.RentalRideBooking where
  fromTEntity entity = do
    let RentalRideBookingT {..} = entityVal entity
    return $
      Domain.RentalRideBooking
        { rideBookingId = Id rideBookingId,
          rentalFarePolicyId = fromKey rentalFarePolicyId
        }
  toTType Domain.RentalRideBooking {..} =
    RentalRideBookingT
      { rideBookingId = getId rideBookingId,
        rentalFarePolicyId = toKey rentalFarePolicyId
      }
  toTEntity a =
    Entity (toKey a.rideBookingId) $ toTType a
