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

Module      :  App.Routes

Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module App.Routes
  ( MockFcmAPI,
    mockFcmAPI,
    mockFcmServer,
  )
where

import App.Types
import Beckn.External.FCM.Flow as API
import Beckn.Types.App (FlowServerR)
import EulerHS.Prelude
import qualified Product.Fcm as P
import Servant hiding (throwError)
import Types.API.Fcm as API

type MockFcmAPI =
  Get '[JSON] Text
    :<|> API.FCMSendMessageAPI
    :<|> API.ReadFcmAPI

mockFcmAPI :: Proxy MockFcmAPI
mockFcmAPI = Proxy

mockFcmServer :: FlowServerR AppEnv MockFcmAPI
mockFcmServer =
  pure "MockFcm is UP"
    :<|> P.sendFcm
    :<|> P.readFcm
