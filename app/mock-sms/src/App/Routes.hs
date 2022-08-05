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
  ( MockSmsAPI,
    mockSmsAPI,
    mockSmsServer,
  )
where

import App.Types
import qualified Beckn.External.MyValueFirst.API as API
import Beckn.Types.App (FlowServerR)
import EulerHS.Prelude
import qualified Product.Sms as P
import Servant hiding (throwError)
import qualified Types.API.Sms as API

type MockSmsAPI =
  Get '[JSON] Text
    :<|> API.ServiceAPI
    :<|> API.ReadSmsAPI

mockSmsAPI :: Proxy MockSmsAPI
mockSmsAPI = Proxy

mockSmsServer :: FlowServerR AppEnv MockSmsAPI
mockSmsServer =
  pure "MockSms is UP"
    :<|> P.sendSms
    :<|> P.readSms
