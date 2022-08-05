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
  )
where

import Beckn.Storage.Esqueleto (Transactionable)
import Beckn.Types.Id
import Domain.Types.FarePolicy (FarePolicy)
import Domain.Types.Organization (Organization)
import EulerHS.Prelude hiding (id)
import Product.FareCalculator.Calculator
  ( FareParameters (..),
    calculateFareParameters,
    fareSum,
  )
import qualified Storage.Queries.FarePolicy as FarePolicyS
import Types.Error
import Utils.Common

type MonadHandler m = (MonadThrow m, Log m)

newtype ServiceHandle m = ServiceHandle
  { getFarePolicy :: Id Organization -> m (Maybe FarePolicy)
  }

serviceHandle :: Transactionable m => ServiceHandle m
serviceHandle =
  ServiceHandle
    { getFarePolicy = \orgId -> do
        FarePolicyS.findFarePolicyByOrg orgId
    }

calculateFare ::
  (Transactionable m, MonadFlow m) =>
  Id Organization ->
  Meter ->
  UTCTime ->
  m FareParameters
calculateFare = doCalculateFare serviceHandle

doCalculateFare ::
  MonadHandler m =>
  ServiceHandle m ->
  Id Organization ->
  Meter ->
  UTCTime ->
  m FareParameters
doCalculateFare ServiceHandle {..} orgId distance startTime = do
  logTagInfo "FareCalculator" $ "Initiating fare calculation for organization " +|| orgId ||+ ""
  farePolicy <- getFarePolicy orgId >>= fromMaybeM NoFarePolicy
  let fareParams = calculateFareParameters farePolicy distance startTime
  logTagInfo
    "FareCalculator"
    $ "Fare parameters calculated: " +|| fareParams ||+ ""
  pure fareParams
