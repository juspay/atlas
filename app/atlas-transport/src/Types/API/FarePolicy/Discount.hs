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

Module      :  Types.API.FarePolicy.Discount

Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Types.API.FarePolicy.Discount
  ( UpdateFarePolicyDiscountReq,
    UpdateFarePolicyDiscountRes,
    CreateFarePolicyDiscountReq,
    CreateFarePolicyDiscountRes,
    DeleteFarePolicyDiscountRes,
    validateCreateFarePolicyDiscountReq,
    validateUpdateFarePolicyDiscountReq,
  )
where

import Beckn.Types.APISuccess
import Beckn.Types.Predicate
import Beckn.Utils.Validation
import Data.OpenApi (ToSchema)
import Data.Time (UTCTime)
import qualified Domain.Types.Vehicle as Veh
import EulerHS.Prelude hiding (id)
import qualified Utils.Validation as TV

data CreateFarePolicyDiscountReq = CreateFarePolicyDiscountReq
  { vehicleVariant :: Veh.Variant,
    fromDate :: UTCTime,
    toDate :: UTCTime,
    discount :: Double,
    enabled :: Bool
  }
  deriving (Generic, Show, FromJSON, ToSchema)

type CreateFarePolicyDiscountRes = APISuccess

validateCreateFarePolicyDiscountReq :: Validate CreateFarePolicyDiscountReq
validateCreateFarePolicyDiscountReq CreateFarePolicyDiscountReq {..} =
  sequenceA_
    [ validateField "discount" discount $ Min @Double 0.01,
      TV.validateDiscountDate "toDate" toDate fromDate
    ]

data UpdateFarePolicyDiscountReq = UpdateFarePolicyDiscountReq
  { fromDate :: UTCTime,
    toDate :: UTCTime,
    discount :: Double,
    enabled :: Bool
  }
  deriving (Generic, Show, FromJSON, ToSchema)

type UpdateFarePolicyDiscountRes = APISuccess

validateUpdateFarePolicyDiscountReq :: Validate UpdateFarePolicyDiscountReq
validateUpdateFarePolicyDiscountReq UpdateFarePolicyDiscountReq {..} =
  sequenceA_
    [ validateField "discount" discount $ Min @Double 0.01,
      TV.validateDiscountDate "toDate" toDate fromDate
    ]

type DeleteFarePolicyDiscountRes = APISuccess
