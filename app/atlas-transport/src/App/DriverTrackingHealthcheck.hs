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

Module      :  App.DriverTrackingHealthcheck
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module App.DriverTrackingHealthcheck where

import App.DriverTrackingHealthcheck.Config
import App.DriverTrackingHealthcheck.Environment
import Beckn.Exit
import Beckn.Storage.Redis.Config
import qualified Beckn.Tools.Metrics.Init as Metrics
import qualified Beckn.Types.App as App
import Beckn.Types.Flow
import Beckn.Utils.App
import Beckn.Utils.Dhall (readDhallConfigDefault)
import qualified Beckn.Utils.FlowLogging as L
import qualified Beckn.Utils.Servant.Server as Server
import Beckn.Utils.Shutdown
import Control.Concurrent
import EulerHS.Prelude hiding (exitSuccess)
import qualified EulerHS.Runtime as R
import Network.Wai.Handler.Warp
import Product.HealthCheck
import Servant
import qualified Services.DriverTrackingHealthcheck as Service
import Utils.Common

runDriverHealthcheck :: (AppCfg -> AppCfg) -> IO ()
runDriverHealthcheck configModifier = do
  config <- configModifier <$> readDhallConfigDefault "driver-tracking-healthcheck-service"
  Metrics.serve config.metricsPort
  hostname <- getPodName
  let loggerRt = L.getEulerLoggerRuntime hostname config.loggerConfig
  appEnv <- buildAppEnv config

  R.withFlowRuntime (Just loggerRt) \flowRt -> do
    flowRt' <- runFlowR flowRt appEnv do
      _ <-
        try (prepareRedisConnections config.redisCfg)
          >>= handleLeft @SomeException exitConnCheckFailure "Connections check failed. Exception thrown: "
      managers <- createManagers mempty -- default manager is created
      pure $ flowRt {R._httpClientManagers = managers}

    let settings =
          defaultSettings
            & setGracefulShutdownTimeout (Just $ getSeconds config.graceTerminationPeriod)
            & setInstallShutdownHandler (handleShutdown appEnv.isShuttingDown (releaseAppEnv appEnv))
            & setPort config.healthcheckPort
    void . forkIO . runSettings settings $
      Server.run healthCheckAPI (healthCheck "driver-tracking-healthcheck") EmptyContext (App.EnvR flowRt' appEnv)

    runFlowR flowRt' appEnv Service.driverTrackingHealthcheckService
    waitForShutdown appEnv.isShuttingDown
