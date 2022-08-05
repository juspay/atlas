{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}


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

Module      :  Storage.Tabular.FarePolicy.Discount
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Storage.Tabular.FarePolicy.Discount where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import qualified Domain.Types.FarePolicy.Discount as Domain
import qualified Domain.Types.FareProduct as DFareProduct
import qualified Domain.Types.Vehicle as DVeh
import qualified Storage.Tabular.Organization as TOrg
import Storage.Tabular.Vehicle ()

mkPersist
  defaultSqlSettings
  [defaultQQ|
    DiscountT sql=fare_policy_discount
      id Text
      vehicleVariant DVeh.Variant
      organizationId TOrg.OrganizationTId
      fareProductType DFareProduct.FareProductType
      fromDate UTCTime
      toDate UTCTime
      enabled Bool
      discount Double
      createdAt UTCTime
      updatedAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey DiscountT where
  type DomainKey DiscountT = Id Domain.Discount
  fromKey (DiscountTKey _id) = Id _id
  toKey (Id id) = DiscountTKey id

instance TEntity DiscountT Domain.Discount where
  fromTEntity entity = do
    let DiscountT {..} = entityVal entity
    return $
      Domain.Discount
        { id = Id id,
          organizationId = fromKey organizationId,
          discount = toRational discount,
          ..
        }
  toTType Domain.Discount {..} =
    DiscountT
      { id = getId id,
        organizationId = toKey organizationId,
        discount = fromRational discount,
        ..
      }
  toTEntity a =
    Entity (toKey a.id) $ toTType a
