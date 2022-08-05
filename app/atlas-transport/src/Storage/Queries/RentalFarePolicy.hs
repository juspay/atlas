{-# LANGUAGE TypeApplications #-}


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

Module      :  Storage.Queries.RentalFarePolicy
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Storage.Queries.RentalFarePolicy where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Amount
import Beckn.Types.Id
import Domain.Types.Organization
import Domain.Types.RentalFarePolicy
import qualified Domain.Types.RentalFarePolicy as Domain
import Domain.Types.Vehicle as Vehicle
import Storage.Tabular.RentalFarePolicy

create ::
  Domain.RentalFarePolicy ->
  SqlDB ()
create = Esq.create'

-- it's possible to find deleted fare policies only by their id.
-- other function return only not deleted fare policies
-- (RentalFarePolicyDeleted ==. val False)

findById :: Transactionable m => Id RentalFarePolicy -> m (Maybe RentalFarePolicy)
findById = Esq.findById

findRentalFarePoliciesByOrg ::
  Transactionable m =>
  Id Organization ->
  m [RentalFarePolicy]
findRentalFarePoliciesByOrg orgId = do
  Esq.findAll $ do
    rentalFarePolicy <- from $ table @RentalFarePolicyT
    where_ $
      rentalFarePolicy ^. RentalFarePolicyOrganizationId ==. val (toKey orgId)
        &&. rentalFarePolicy ^. RentalFarePolicyDeleted ==. val False
    return rentalFarePolicy

markAllAsDeleted ::
  Id Organization ->
  SqlDB ()
markAllAsDeleted orgId = Esq.update' $ \rentalFp -> do
  set
    rentalFp
    [RentalFarePolicyDeleted =. val True]
  where_ $ rentalFp ^. RentalFarePolicyOrganizationId ==. val (toKey orgId)

findRentalFarePolicyForQuote ::
  Transactionable m =>
  Id Organization ->
  Vehicle.Variant ->
  Amount ->
  m (Maybe RentalFarePolicy)
findRentalFarePolicyForQuote orgId vehicleVariant_ baseFare = do
  Esq.findOne $ do
    rentalFarePolicy <- from $ table @RentalFarePolicyT
    where_ $
      rentalFarePolicy ^. RentalFarePolicyOrganizationId ==. val (toKey orgId)
        &&. rentalFarePolicy ^. RentalFarePolicyVehicleVariant ==. val vehicleVariant_
        &&. rentalFarePolicy ^. RentalFarePolicyBaseFare ==. val baseFare
    return rentalFarePolicy
