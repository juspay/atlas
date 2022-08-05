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

Module      :  Mobility.HealthCheck
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Mobility.HealthCheck where

import Common (getAppBaseUrl)
import Data.Text.Encoding as DT
import EulerHS.Prelude
import Mobility.Fixtures
import qualified Network.HTTP.Client as Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant hiding (Context)
import Servant.Client
import Test.Hspec
import Utils

type HealthCheckAPI = Get '[JSON] Text

healthCheckBackendC :: ClientM Text
healthCheckBackendC = client (Proxy :: Proxy HealthCheckAPI)

spec :: Spec
spec = do
  appManager <- runIO $ Client.newManager tlsManagerSettings
  let appBaseUrl = getAppBaseUrl
      transporterBaseUrl = getTransporterBaseUrl
      appClientEnv = mkClientEnv appManager appBaseUrl
      tbeClientEnv = mkClientEnv appManager transporterBaseUrl
      gatewayClientEnv =
        mkClientEnv appManager $
          transporterBaseUrl
            { baseUrlPort = 8015,
              baseUrlPath = "/v1"
            }
  describe "Testing App Backend APIs" $
    it "Testing health check API" $
      hspec $
        it "Health Check API should return success" do
          appResult <- runClient appClientEnv healthCheckBackendC
          appResult `shouldBe` Right (DT.decodeUtf8 "App is UP")
          tbeResult <- runClient tbeClientEnv healthCheckBackendC
          tbeResult `shouldBe` Right (DT.decodeUtf8 "App is UP")
          gwResult <- runClient gatewayClientEnv healthCheckBackendC
          gwResult `shouldBe` Right (DT.decodeUtf8 "Gateway is UP")
