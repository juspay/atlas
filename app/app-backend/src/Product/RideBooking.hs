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

Module      :  Product.RideBooking
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Product.RideBooking where

import App.Types
import Beckn.Types.Id
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Ride as SRide
import qualified Domain.Types.RideBooking as SRB
import qualified Domain.Types.SearchReqLocation as SLoc
import EulerHS.Prelude hiding (id)
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RideBooking as QRB
import qualified Storage.Queries.SearchReqLocation as QLoc
import qualified Types.API.RideBooking as API
import Types.Error
import Utils.Common

rideBookingStatus :: Id SRB.RideBooking -> Id Person.Person -> FlowHandler API.RideBookingStatusRes
rideBookingStatus rideBookingId personId = withFlowHandlerAPI $ do
  rideBooking <- QRB.findById rideBookingId >>= fromMaybeM (RideBookingDoesNotExist rideBookingId.getId)
  unless (rideBooking.riderId == personId) $ throwError AccessDenied
  buildRideBookingStatusRes rideBooking

rideBookingList :: Id Person.Person -> Maybe Integer -> Maybe Integer -> Maybe Bool -> FlowHandler API.RideBookingListRes
rideBookingList personId mbLimit mbOffset mbOnlyActive = withFlowHandlerAPI $ do
  rbList <- QRB.findAllByRiderIdAndRide personId mbLimit mbOffset mbOnlyActive
  API.RideBookingListRes <$> traverse buildRideBookingStatusRes rbList

buildRideBookingStatusRes :: EsqDBFlow m r => SRB.RideBooking -> m API.RideBookingStatusRes
buildRideBookingStatusRes rideBooking = do
  fromLocation <- QLoc.findById rideBooking.fromLocationId >>= fromMaybeM LocationNotFound
  mbToLocation <- forM rideBooking.toLocationId (QLoc.findById >=> fromMaybeM LocationNotFound)
  let rbStatus = rideBooking.status
  rideAPIEntityList <-
    QRide.findAllByRBId rideBooking.id
      <&> fmap SRide.makeRideAPIEntity

  return $
    API.RideBookingStatusRes
      { id = rideBooking.id,
        status = rbStatus,
        agencyName = rideBooking.providerName,
        agencyNumber = rideBooking.providerMobileNumber,
        estimatedFare = rideBooking.estimatedFare,
        discount = rideBooking.discount,
        estimatedTotalFare = rideBooking.estimatedTotalFare,
        toLocation = SLoc.makeSearchReqLocationAPIEntity <$> mbToLocation,
        fromLocation = SLoc.makeSearchReqLocationAPIEntity fromLocation,
        rideList = rideAPIEntityList,
        createdAt = rideBooking.createdAt,
        updatedAt = rideBooking.updatedAt
      }
