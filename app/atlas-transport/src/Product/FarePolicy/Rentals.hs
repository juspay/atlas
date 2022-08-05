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

Module      :  Product.FarePolicy.Rentals
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Product.FarePolicy.Rentals where

import App.Types
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.APISuccess
import Beckn.Types.Id (Id (..))
import Beckn.Utils.Validation (runRequestValidation)
import Domain.Types.Organization
import qualified Domain.Types.Person as SP
import Domain.Types.RentalFarePolicy as Domain
import EulerHS.Prelude
import qualified Storage.Queries.RentalFarePolicy as SRentalFarePolicy
import Types.API.FarePolicy.Rentals
import Types.Error
import Utils.Common

createRentalFarePolicy :: SP.Person -> CreateRentalFarePolicyReq -> FlowHandler APISuccess
createRentalFarePolicy admin req = withFlowHandlerAPI $ do
  orgId <- admin.organizationId & fromMaybeM (PersonFieldNotPresent "organizationId")
  mapM_ (runRequestValidation validateCreateRentalsFarePolicyRequest) req.createList
  newRentalFarePolicyItems <- forM req.createList $ \createItemReq -> do
    guid <- Id <$> generateGUID
    pure $ toDomainType orgId guid createItemReq
  Esq.runTransaction $ do
    SRentalFarePolicy.markAllAsDeleted orgId
    forM_ newRentalFarePolicyItems SRentalFarePolicy.create
  pure Success
  where
    toDomainType :: Id Organization -> Id RentalFarePolicy -> CreateRentalFarePolicyItem -> RentalFarePolicy
    toDomainType orgId guid CreateRentalFarePolicyItem {..} = do
      RentalFarePolicy
        { id = guid,
          organizationId = orgId,
          baseFare = realToFrac baseFare,
          extraKmFare = realToFrac extraKmFare,
          extraMinuteFare = realToFrac extraMinuteFare,
          driverAllowanceForDay = realToFrac <$> driverAllowanceForDay,
          ..
        }

listRentalFarePolicies :: SP.Person -> FlowHandler ListRentalFarePoliciesRes
listRentalFarePolicies person = withFlowHandlerAPI $ do
  orgId <- person.organizationId & fromMaybeM (PersonFieldNotPresent "organizationId")
  rentalFarePolicies <- SRentalFarePolicy.findRentalFarePoliciesByOrg orgId
  pure $
    ListRentalFarePoliciesRes
      { rentalFarePolicies = map makeRentalFarePolicyAPIEntity rentalFarePolicies
      }
