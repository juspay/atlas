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

Module      :  Domain.Types.FarePolicy.Discount
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Domain.Types.FarePolicy.Discount where

import Beckn.Types.Id (Id)
import Beckn.Types.Predicate
import Beckn.Utils.Validation
import Data.OpenApi (ToSchema)
import Data.Time (UTCTime)
import qualified Domain.Types.FareProduct as DFareProduct
import qualified Domain.Types.Organization as DOrg
import qualified Domain.Types.Vehicle as DVeh
import EulerHS.Prelude hiding (id)
import Storage.Tabular.FareProduct ()

data Discount = Discount
  { id :: Id Discount,
    vehicleVariant :: DVeh.Variant,
    organizationId :: Id DOrg.Organization,
    fareProductType :: DFareProduct.FareProductType,
    fromDate :: UTCTime,
    toDate :: UTCTime,
    discount :: Rational,
    enabled :: Bool,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show, Eq)

data DiscountAPIEntity = DiscountAPIEntity
  { id :: Id Discount,
    fromDate :: UTCTime,
    toDate :: UTCTime,
    discount :: Double,
    enabled :: Bool
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON, ToSchema)

makeDiscountAPIEntity :: Discount -> DiscountAPIEntity
makeDiscountAPIEntity Discount {..} =
  DiscountAPIEntity
    { discount = fromRational discount,
      ..
    }

validateDiscountAPIEntity :: Validate DiscountAPIEntity
validateDiscountAPIEntity discountApiEntity =
  validateField "discount" discountApiEntity.discount $ Min @Double 0.01
