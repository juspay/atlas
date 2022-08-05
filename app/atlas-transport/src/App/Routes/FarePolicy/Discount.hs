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

Module      :  App.Routes.FarePolicy.Discount
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module App.Routes.FarePolicy.Discount where

import App.Types
import Beckn.Types.Id (Id)
import Domain.Types.FarePolicy.Discount (Discount)
import Product.FarePolicy.Discount
import Servant
import Types.API.FarePolicy.Discount
import Utils.Auth

type FPDiscountAPI =
  "discount"
    :> ( AdminTokenAuth
           :> ReqBody '[JSON] CreateFarePolicyDiscountReq
           :> Post '[JSON] CreateFarePolicyDiscountRes
           :<|> AdminTokenAuth
             :> Capture "discountId" (Id Discount)
             :> ReqBody '[JSON] UpdateFarePolicyDiscountReq
             :> Post '[JSON] UpdateFarePolicyDiscountRes
           :<|> AdminTokenAuth
             :> Capture "discountId" (Id Discount)
             :> Delete '[JSON] DeleteFarePolicyDiscountRes
       )

discountFlow :: FlowServer FPDiscountAPI
discountFlow =
  createFarePolicyDiscount
    :<|> updateFarePolicyDiscount
    :<|> deleteFarePolicyDiscount
