{-# LANGUAGE UndecidableInstances #-}


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

Module      :  Domain.Types.RideBookingCancellationReason

Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Domain.Types.RideBookingCancellationReason
  ( module Reexport,
    module Domain.Types.RideBookingCancellationReason,
  )
where

import Beckn.Types.Core.Taxi.Common.CancellationSource as Reexport (CancellationSource (..))
import Beckn.Types.Id
import Domain.Types.CancellationReason (CancellationReasonCode)
import Domain.Types.Person (Person)
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.RideBooking as DRB
import EulerHS.Prelude hiding (id)

data RideBookingCancellationReason = RideBookingCancellationReason
  { id :: Id RideBookingCancellationReason,
    driverId :: Maybe (Id Person),
    rideBookingId :: Id DRB.RideBooking,
    rideId :: Maybe (Id DRide.Ride),
    source :: CancellationSource,
    reasonCode :: Maybe CancellationReasonCode,
    additionalInfo :: Maybe Text
  }
  deriving (Generic)
