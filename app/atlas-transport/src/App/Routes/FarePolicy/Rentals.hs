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

Module      :  App.Routes.FarePolicy.Rentals
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module App.Routes.FarePolicy.Rentals where

import App.Types
import Beckn.Types.APISuccess (APISuccess)
import Product.FarePolicy.Rentals
import Servant
import Types.API.FarePolicy.Rentals
import Utils.Auth

-- rentals fare policies
type FPRentalsAPI =
  "rentals"
    :> AdminTokenAuth
    :> ( ListRentalFarePoliciesAPI
           :<|> CreateRentalFarePoliciesAPI
       )

type ListRentalFarePoliciesAPI =
  Get '[JSON] ListRentalFarePoliciesRes

type CreateRentalFarePoliciesAPI =
  ReqBody '[JSON] CreateRentalFarePolicyReq
    :> Post '[JSON] APISuccess

fpRentalsFlow :: FlowServer FPRentalsAPI
fpRentalsFlow p =
  listRentalFarePolicies p
    :<|> createRentalFarePolicy p
