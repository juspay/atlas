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

Module      :  Product.Ride
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Product.Ride where

import App.Types
import Beckn.Prelude
import Beckn.Types.Id
import qualified Domain.Types.Person as SP
import qualified Domain.Types.Ride as SRide
import qualified Domain.Types.RideBooking as SRB
import qualified Domain.Types.SearchReqLocation as SLoc
import qualified Product.RideBooking as RideBooking (getDropLocation)
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.SearchReqLocation as QLoc
import qualified Storage.Queries.Vehicle as QVeh
import qualified Types.API.Ride as API
import Types.Error
import Utils.Common

listDriverRides ::
  Id SP.Person ->
  Maybe Integer ->
  Maybe Integer ->
  Maybe Bool ->
  FlowHandler API.DriverRideListRes
listDriverRides driverId mbLimit mbOffset mbOnlyActive = withFlowHandlerAPI $ do
  rideData <- QRide.findAllByDriverId driverId mbLimit mbOffset mbOnlyActive
  API.DriverRideListRes <$> traverse buildDriverRideRes rideData

buildDriverRideRes :: (SRide.Ride, SRB.RideBooking) -> Flow API.DriverRideRes
buildDriverRideRes (ride, rideBooking) = do
  fromLocation <- QLoc.findById rideBooking.fromLocationId >>= fromMaybeM LocationNotFound
  toLocation <- RideBooking.getDropLocation rideBooking.rideBookingDetails

  vehicle <- QVeh.findById ride.vehicleId >>= fromMaybeM (VehicleNotFound ride.vehicleId.getId)
  driver <- QP.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
  driverNumber <- SP.getPersonNumber driver
  pure
    API.DriverRideRes
      { id = ride.id,
        shortRideId = ride.shortId,
        status = ride.status,
        fromLocation = SLoc.makeSearchReqLocationAPIEntity fromLocation,
        toLocation = SLoc.makeSearchReqLocationAPIEntity <$> toLocation,
        estimatedFare = rideBooking.estimatedFare,
        estimatedTotalFare = rideBooking.estimatedTotalFare,
        discount = rideBooking.discount,
        driverName = driver.firstName,
        driverNumber = driverNumber,
        vehicleNumber = vehicle.registrationNo,
        vehicleColor = vehicle.color,
        vehicleVariant = vehicle.variant,
        vehicleModel = vehicle.model,
        computedFare = ride.fare,
        computedTotalFare = ride.totalFare,
        actualRideDistance = ride.traveledDistance,
        createdAt = ride.createdAt,
        updatedAt = ride.updatedAt
      }
