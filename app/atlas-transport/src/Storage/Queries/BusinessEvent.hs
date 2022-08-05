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

Module      :  Storage.Queries.BusinessEvent
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Storage.Queries.BusinessEvent where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.BusinessEvent
import Domain.Types.Ride
import Domain.Types.RideBooking
import Domain.Types.Vehicle (Variant)
import Storage.Queries.Person (DriverPoolResult)
import Storage.Tabular.BusinessEvent ()
import Types.App (Driver)
import Utils.Common

logBusinessEvent ::
  Maybe (Id Driver) ->
  EventType ->
  Maybe (Id RideBooking) ->
  Maybe WhenPoolWasComputed ->
  Maybe Variant ->
  Maybe Double ->
  Maybe Double ->
  Maybe (Id Ride) ->
  SqlDB ()
logBusinessEvent driverId eventType rideBookingId whenPoolWasComputed variant distance duration rideId = do
  uuid <- generateGUID
  now <- getCurrentTime
  Esq.create' $
    BusinessEvent
      { id = uuid,
        eventType = eventType,
        timeStamp = now,
        driverId = driverId,
        rideBookingId = rideBookingId,
        whenPoolWasComputed = whenPoolWasComputed,
        vehicleVariant = variant,
        distance = distance,
        duration = duration,
        rideId = rideId
      }

logDriverInPoolEvent :: WhenPoolWasComputed -> Maybe (Id RideBooking) -> DriverPoolResult -> SqlDB ()
logDriverInPoolEvent whenPoolWasComputed rideBookingId driverInPool = do
  logBusinessEvent
    (Just driverInPool.driverId)
    DRIVER_IN_POOL
    rideBookingId
    (Just whenPoolWasComputed)
    (Just driverInPool.variant)
    (Just driverInPool.distanceToDriver)
    driverInPool.durationToPickup
    Nothing

logDriverAssignetEvent :: Id Driver -> Id RideBooking -> Id Ride -> SqlDB ()
logDriverAssignetEvent driverId rideBookingId rideId = do
  logBusinessEvent
    (Just driverId)
    DRIVER_ASSIGNED
    (Just rideBookingId)
    Nothing
    Nothing
    Nothing
    Nothing
    (Just rideId)

logRideConfirmedEvent :: Id RideBooking -> SqlDB ()
logRideConfirmedEvent rideBookingId = do
  logBusinessEvent
    Nothing
    RIDE_CONFIRMED
    (Just rideBookingId)
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing

logRideCommencedEvent :: Id Driver -> Id RideBooking -> Id Ride -> SqlDB ()
logRideCommencedEvent driverId rideBookingId rideId = do
  logBusinessEvent
    (Just driverId)
    RIDE_COMMENCED
    (Just rideBookingId)
    Nothing
    Nothing
    Nothing
    Nothing
    (Just rideId)
