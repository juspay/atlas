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

Module      :  Storage.Queries.FarePolicy
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Storage.Queries.FarePolicy where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.FarePolicy
import Domain.Types.FarePolicy.PerExtraKmRate (PerExtraKmRate)
import Domain.Types.Organization
import Domain.Types.Vehicle as Vehicle
import qualified Storage.Queries.FarePolicy.PerExtraKmRate as QExtraKmRate
import Storage.Tabular.FarePolicy
import Utils.Common

findFarePolicyByOrgAndVehicleVariant ::
  Transactionable m =>
  Id Organization ->
  Vehicle.Variant ->
  m (Maybe FarePolicy)
findFarePolicyByOrgAndVehicleVariant orgId vehicleVariant_ = do
  Esq.findOne $ do
    farePolicy <- from $ table @FarePolicyT
    where_ $
      farePolicy ^. FarePolicyOrganizationId ==. val (toKey orgId)
        &&. farePolicy ^. FarePolicyVehicleVariant ==. val vehicleVariant_
    return farePolicy

findFarePoliciesByOrgId :: Transactionable m => Id Organization -> m [FarePolicy]
findFarePoliciesByOrgId orgId = do
  Esq.findAll $ do
    farePolicy <- from $ table @FarePolicyT
    where_ $ farePolicy ^. FarePolicyOrganizationId ==. val (toKey orgId)
    orderBy [asc $ farePolicy ^. FarePolicyVehicleVariant]
    return farePolicy

findById :: Transactionable m => Id FarePolicy -> m (Maybe FarePolicy)
findById = Esq.findById

updateFarePolicy :: FarePolicy -> SqlDB ()
updateFarePolicy farePolicy = do
  now <- getCurrentTime
  void $
    upsert'
      farePolicy
      [ FarePolicyBaseFare =. val (fromRational <$> farePolicy.baseFare),
        FarePolicyNightShiftStart =. val (farePolicy.nightShiftStart),
        FarePolicyNightShiftEnd =. val (farePolicy.nightShiftEnd),
        FarePolicyNightShiftRate =. val (fromRational <$> farePolicy.nightShiftRate),
        FarePolicyUpdatedAt =. val now
      ]
  QExtraKmRate.deleteAll farePolicy.organizationId farePolicy.vehicleVariant
  perExtraKmRateList <- mapM (buildPerExtraKmRate farePolicy) farePolicy.perExtraKmRateList
  create' `mapM_` perExtraKmRateList
  where
    buildPerExtraKmRate FarePolicy {..} perExtraKmRate = do
      uuid <- generateGUID
      return (Id uuid :: Id PerExtraKmRate, organizationId, vehicleVariant, perExtraKmRate)
