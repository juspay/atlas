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

Module      :  Domain.Types.RentalFarePolicy
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Domain.Types.RentalFarePolicy where

import Beckn.Prelude
import Beckn.Types.Amount
import Beckn.Types.Id (Id)
import qualified Domain.Types.Organization as Organization
import qualified Domain.Types.Vehicle as Vehicle

data RentalFarePolicy = RentalFarePolicy
  { id :: Id RentalFarePolicy,
    organizationId :: Id Organization.Organization,
    vehicleVariant :: Vehicle.Variant,
    baseFare :: Amount,
    baseDistance :: Double, -- Distance
    baseDurationHr :: Int,
    extraKmFare :: Amount,
    extraMinuteFare :: Amount,
    driverAllowanceForDay :: Maybe Amount
  }
  deriving (Generic, Show, Eq)

data RentalFarePolicyAPIEntity = RentalFarePolicyAPIEntity
  { id :: Id RentalFarePolicy,
    vehicleVariant :: Vehicle.Variant,
    baseFare :: Amount,
    baseDistance :: Double, -- Distance
    baseDurationHr :: Int,
    extraKmFare :: Amount,
    extraMinuteFare :: Amount,
    driverAllowanceForDay :: Maybe Amount
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

makeRentalFarePolicyAPIEntity :: RentalFarePolicy -> RentalFarePolicyAPIEntity
makeRentalFarePolicyAPIEntity RentalFarePolicy {..} = RentalFarePolicyAPIEntity {..}
