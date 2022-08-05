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

Module      :  App.Routes.FarePolicy
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module App.Routes.FarePolicy where

import App.Routes.FarePolicy.Discount
import App.Routes.FarePolicy.FareProduct
import App.Routes.FarePolicy.Rentals
import App.Types
import Beckn.Types.Id (Id)
import Domain.Types.FarePolicy (FarePolicy)
import Product.FarePolicy
import Servant
import Types.API.FarePolicy
import Utils.Auth

-- total
type FarePolicyAPI =
  "org"
    :> ( FareProductAPI
           :<|> "farePolicy"
           :> ( FPDiscountAPI
                  :<|> FPRentalsAPI
                  :<|> AdminTokenAuth :> Get '[JSON] ListFarePolicyRes
                  :<|> AdminTokenAuth
                    :> Capture "farePolicyId" (Id FarePolicy)
                    :> ReqBody '[JSON] UpdateFarePolicyReq
                    :> Post '[JSON] UpdateFarePolicyRes
              )
       )

farePolicyFlow :: FlowServer FarePolicyAPI
farePolicyFlow =
  fareProductFlow
    :<|> ( discountFlow
             :<|> fpRentalsFlow
             :<|> listFarePolicies
             :<|> updateFarePolicy
         )
