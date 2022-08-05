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

Module      :  Product.FarePolicy.FareProduct
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Product.FarePolicy.FareProduct where

import App.Types
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.APISuccess
import Domain.Types.FareProduct
import qualified Domain.Types.Person as SP
import EulerHS.Prelude
import qualified Storage.Queries.FarePolicy.FareProduct as SFareProduct
import Types.API.FarePolicy.FareProduct
import Types.Error
import Utils.Common

listFareProducts :: SP.Person -> FlowHandler ListFareProductsRes
listFareProducts person = withFlowHandlerAPI $ do
  orgId <- person.organizationId & fromMaybeM (PersonFieldNotPresent "organizationId")
  fareProducts <- SFareProduct.findEnabledByOrgId orgId
  pure $ ListFareProductsRes $ makeFareProductAPIEntity <$> fareProducts

updateFareProduct :: SP.Person -> UpdateFareProductReq -> FlowHandler APISuccess
updateFareProduct person updReq = withFlowHandlerAPI $ do
  orgId <- person.organizationId & fromMaybeM (PersonFieldNotPresent "organizationId")
  Esq.runTransaction $
    if updReq.enabled
      then SFareProduct.upsertFareProduct orgId updReq.fareProductType
      else SFareProduct.deleteFareProduct orgId updReq.fareProductType
  pure Success
