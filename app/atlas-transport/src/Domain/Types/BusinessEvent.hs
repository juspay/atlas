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

Module      :  Domain.Types.BusinessEvent
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Domain.Types.BusinessEvent where

import Beckn.Prelude
import Beckn.Types.Id (Id)
import Domain.Types.Ride (Ride)
import Domain.Types.RideBooking (RideBooking)
import Domain.Types.Vehicle (Variant)
import Types.App (Driver)

data BusinessEvent = BusinessEvent
  { id :: Id BusinessEvent,
    driverId :: Maybe (Id Driver),
    eventType :: EventType,
    timeStamp :: UTCTime,
    rideBookingId :: Maybe (Id RideBooking),
    whenPoolWasComputed :: Maybe WhenPoolWasComputed,
    vehicleVariant :: Maybe Variant,
    distance :: Maybe Double,
    duration :: Maybe Double,
    rideId :: Maybe (Id Ride)
  }
  deriving (Generic)

data EventType = DRIVER_IN_POOL | RIDE_COMMENCED | DRIVER_ASSIGNED | RIDE_CONFIRMED
  deriving (Show, Eq, Read, Generic, ToSchema)

data WhenPoolWasComputed = ON_SEARCH | ON_CONFIRM | ON_REALLOCATION
  deriving (Show, Eq, Read, Generic, ToSchema)
