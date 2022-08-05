{-# LANGUAGE DerivingVia #-}


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

Module      :  Types.API.FarePolicy.Rentals
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Types.API.FarePolicy.Rentals where

import Beckn.Prelude
import Beckn.Types.Predicate
import Beckn.Utils.Validation (Validate, validateField)
import Domain.Types.RentalFarePolicy (RentalFarePolicyAPIEntity)
import qualified Domain.Types.Vehicle as Vehicle

newtype ListRentalFarePoliciesRes = ListRentalFarePoliciesRes
  { rentalFarePolicies :: [RentalFarePolicyAPIEntity]
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype CreateRentalFarePolicyReq = CreateRentalFarePolicyReq
  { createList :: NonEmpty CreateRentalFarePolicyItem
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CreateRentalFarePolicyItem = CreateRentalFarePolicyItem
  { vehicleVariant :: Vehicle.Variant,
    baseFare :: Double,
    baseDistance :: Double, -- Distance
    baseDurationHr :: Int,
    extraKmFare :: Double,
    extraMinuteFare :: Double,
    driverAllowanceForDay :: Maybe Double
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

validateCreateRentalsFarePolicyRequest :: Validate CreateRentalFarePolicyItem
validateCreateRentalsFarePolicyRequest CreateRentalFarePolicyItem {..} =
  sequenceA_
    [ validateField "baseFare" baseFare $ Min @Double 0,
      validateField "baseDistance" baseDistance $ Min @Double 0,
      validateField "baseDurationHr" baseDurationHr $ Min @Int 0,
      validateField "extraKmFare" extraKmFare $ Min @Double 0,
      validateField "extraMinuteFare" extraMinuteFare $ Min @Double 0,
      validateField "driverAllowanceForDay" driverAllowanceForDay $ InMaybe $ Min @Double 0
    ]
