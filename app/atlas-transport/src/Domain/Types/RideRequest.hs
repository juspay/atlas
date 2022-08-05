{-# LANGUAGE DerivingVia #-}
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

Module      :  Domain.Types.RideRequest
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Domain.Types.RideRequest where

import Beckn.Types.Id
import Beckn.Utils.GenericPretty
import Data.Time (UTCTime)
import Domain.Types.Organization
import qualified Domain.Types.RideBooking as DRB
import EulerHS.Prelude hiding (id)

data RideRequestType = ALLOCATION | CANCELLATION | DRIVER_RESPONSE
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)
  deriving (PrettyShow) via Showable RideRequestType

data RideRequest = RideRequest
  { id :: Id RideRequest,
    rideBookingId :: Id DRB.RideBooking,
    shortOrgId :: ShortId Organization,
    createdAt :: UTCTime,
    _type :: RideRequestType,
    info :: Maybe Text
  }
  deriving (Generic, Eq, Show, ToJSON, FromJSON, PrettyShow)
