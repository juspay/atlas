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

Module      :  Product.FareCalculator.Flow

Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Product.FareCalculator.Flow
  ( FareParameters (..),
    ServiceHandle (..),
    calculateFare,
    doCalculateFare,
    fareSum,
    fareSumWithDiscount,
  )
where

import Beckn.Types.Id
import Domain.Types.FarePolicy (FarePolicy)
import Domain.Types.Organization (Organization)
import qualified Domain.Types.Vehicle as Vehicle
import EulerHS.Prelude hiding (id)
import Product.FareCalculator.Calculator
  ( FareParameters (..),
    calculateFareParameters,
    fareSum,
    fareSumWithDiscount,
  )
import qualified Storage.Queries.FarePolicy as FarePolicyS
import Types.Error
import Utils.Common

type MonadHandler m = (MonadThrow m, Log m)

newtype ServiceHandle m = ServiceHandle
  { getFarePolicy :: Id Organization -> Vehicle.Variant -> m (Maybe FarePolicy)
  }

serviceHandle :: EsqDBFlow m r => ServiceHandle m
serviceHandle =
  ServiceHandle
    { getFarePolicy = \orgId vehicleVariant -> do
        FarePolicyS.findFarePolicyByOrgAndVehicleVariant orgId vehicleVariant
    }

calculateFare ::
  EsqDBFlow m r =>
  Id Organization ->
  Vehicle.Variant ->
  Meter ->
  UTCTime ->
  m FareParameters
calculateFare = doCalculateFare serviceHandle

doCalculateFare ::
  MonadHandler m =>
  ServiceHandle m ->
  Id Organization ->
  Vehicle.Variant ->
  Meter ->
  UTCTime ->
  m FareParameters
doCalculateFare ServiceHandle {..} orgId vehicleVariant distance startTime = do
  logTagInfo "FareCalculator" $ "Initiating fare calculation for organization " +|| orgId ||+ " for " +|| vehicleVariant ||+ ""
  farePolicy <- getFarePolicy orgId vehicleVariant >>= fromMaybeM NoFarePolicy
  let fareParams = calculateFareParameters farePolicy distance startTime
  logTagInfo
    "FareCalculator"
    $ "Fare parameters calculated: " +|| fareParams ||+ ""
  pure fareParams
