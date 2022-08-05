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

Module      :  Fixtures.RideBooking
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Fixtures.RideBooking where

import Beckn.Types.Id
import qualified Domain.Types.RideBooking as SRB
import qualified Domain.Types.Vehicle as Veh
import EulerHS.Prelude
import qualified Fixtures.BaseUrl as Fixtures
import qualified Fixtures.Time as Fixtures

defaultRideBooking :: SRB.RideBooking
defaultRideBooking = do
  let details =
        SRB.OneWayRideBookingDetails
          { toLocationId = Id "",
            estimatedDistance = 0
          }
  SRB.RideBooking
    { id = Id "1",
      messageId = "",
      requestId = Id "1",
      quoteId = "1",
      status = SRB.CONFIRMED,
      providerId = Id "",
      bapId = "",
      bapUri = Fixtures.defaultUrl,
      startTime = Fixtures.defaultTime,
      riderId = Id "",
      fromLocationId = Id "",
      vehicleVariant = Veh.SUV,
      estimatedFare = 0,
      discount = Nothing,
      estimatedTotalFare = 0,
      reallocationsCount = 0,
      rideBookingDetails = SRB.OneWayDetails details,
      createdAt = Fixtures.defaultTime,
      updatedAt = Fixtures.defaultTime
    }
