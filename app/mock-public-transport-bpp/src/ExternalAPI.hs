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

Module      :  ExternalAPI
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module ExternalAPI where

import Beckn.Mock.App
import Beckn.Mock.ExternalAPI
import Beckn.Types.Core.Ack (AckResponse)
import Beckn.Types.Core.ReqTypes
import "public-transport-bap" Core.Spec.OnCancel
import "public-transport-bap" Core.Spec.OnConfirm
import "public-transport-bap" Core.Spec.OnSearch
import "public-transport-bap" Core.Spec.OnStatus
import Environment
import Servant
import Servant.Client

type GatewayOnSearchAPI =
  "on_search"
    :> ReqBody '[JSON] (BecknCallbackReq OnSearchCatalog)
    :> Post '[JSON] AckResponse

callGatewayOnSearch :: BecknCallbackReq OnSearchCatalog -> MockM AppEnv ()
callGatewayOnSearch = callAPI @GatewayOnSearchAPI gatewayUrl
  where
    gatewayUrl = BaseUrl Http "localhost" 8015 "v1"

----------------------------
type OnConfirmAPI =
  "on_confirm"
    :> ReqBody '[JSON] (BecknCallbackReq OnConfirmMessage)
    :> Post '[JSON] AckResponse

callBapOnConfirm :: BecknCallbackReq OnConfirmMessage -> MockM AppEnv ()
callBapOnConfirm = callBapAPI @OnConfirmAPI

----------------------------
type OnStatusAPI =
  "on_status"
    :> ReqBody '[JSON] (BecknCallbackReq OnStatusMessage)
    :> Post '[JSON] AckResponse

callBapOnStatus :: BecknCallbackReq OnStatusMessage -> MockM AppEnv ()
callBapOnStatus = callBapAPI @OnStatusAPI

----------------------------
type OnCancelAPI =
  "on_cancel"
    :> ReqBody '[JSON] (BecknCallbackReq OnCancelMessage)
    :> Post '[JSON] AckResponse

callBapOnCancel :: BecknCallbackReq OnCancelMessage -> MockM AppEnv ()
callBapOnCancel = callBapAPI @OnCancelAPI
