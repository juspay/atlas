{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}


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

Module      :  Main
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Main where

import qualified "app-backend" App as AppBackend
import qualified "atlas-gateway" App as Gateway
import qualified "atlas-transport" App as TransporterBackend
import qualified "mock-fcm" App as MockFcm
import qualified "mock-public-transport-bpp" App as MockPublicTransportBpp
import qualified "mock-registry" App as MockRegistry
import qualified "mock-sms" App as MockSms
import qualified "public-transport-bap" App as PublicTransport
import qualified "public-transport-search-consumer" App as PublicTransportSearchConsumer
import qualified "search-result-aggregator" App as SearchResultAggregator
import qualified "atlas-transport" App.Allocator as Allocator
import qualified "atlas-transport" App.DriverTrackingHealthcheck as DriverHC
import qualified "app-backend" App.Types as AppBackend
import qualified "atlas-transport" App.Types as TransporterBackend
import Beckn.Exit (exitDBMigrationFailure)
import qualified Beckn.Storage.Esqueleto.Migration as Esq
import Beckn.Types.Geofencing
import Beckn.Types.Logging (LoggerConfig)
import Beckn.Utils.App (handleLeft)
import Beckn.Utils.Dhall (readDhallConfigDefault)
import qualified Data.Text as T (replace, toUpper, unpack)
import EulerHS.Prelude
import GHC.Records.Extra (HasField)
import Mobility.Fixtures
import qualified Mobility.Spec as Mobility
import PublicTransport.Common
import qualified PublicTransport.Spec as PublicTransport
import Resources
import System.Environment as Env (setEnv)
import System.Posix
import Test.Tasty
import TestSilentIOLogger ()

main :: IO ()
main = do
  -- We can't really spawn off multiple instances of our servers, so serialise...
  Env.setEnv "TASTY_NUM_THREADS" "1"
  -- Set some config paths in environment...
  mapM_
    setConfigEnv
    [ "allocation-service",
      "app-backend",
      "atlas-gateway",
      "atlas-transport",
      "driver-tracking-healthcheck-service",
      "mock-registry",
      "public-transport-bap",
      "mock-public-transport-bpp",
      "public-transport-search-consumer",
      "search-result-aggregator"
    ]
  -- ... and run
  defaultMain =<< specs
  where
    setConfigEnv app = do
      Env.setEnv
        (T.unpack $ toEnvVar app <> "_CONFIG_PATH")
        (T.unpack $ "../dhall-configs/dev/" <> app <> ".dhall")
      Env.setEnv
        (T.unpack $ toEnvVar app <> "_MIGRATION_PATH")
        (T.unpack $ "../dev/migrations/" <> app)

    toEnvVar = T.toUpper . T.replace "-" "_"

specs :: IO TestTree
specs =
  specs'
    [Mobility.mkTestTree, PublicTransport.mkTestTree]

specs' :: [IO TestTree] -> IO TestTree
specs' trees = do
  readyTests <- sequence trees
  return $
    withResource
      (startServers allServers)
      cleanupServers
      ( \_ ->
          testGroup
            "all"
            readyTests
      )
  where
    allServers =
      [ Allocator.runAllocator \cfg ->
          cfg & hideLogging
            & #driverNotificationExpiry .~ 18
            & #rideAllocationExpiry .~ 18,
        DriverHC.runDriverHealthcheck hideLogging,
        Gateway.runGateway hideLogging,
        AppBackend.runAppBackend $
          \cfg ->
            cfg & hideLogging
              & #geofencingConfig . #origin .~ Regions ["Ernakulam", "Kochi"]
              & #geofencingConfig . #destination .~ Regions ["Kerala", "Kochi"],
        TransporterBackend.runTransporterBackendApp $ \cfg ->
          cfg & hideLogging
            & #updateLocationRefreshPeriod .~ timeBetweenLocationUpdates,
        MockSms.runMockSms hideLogging,
        MockFcm.runMockFcm hideLogging,
        MockRegistry.runRegistryService hideLogging,
        PublicTransport.runService $ \cfg ->
          cfg & hideLogging,
        MockPublicTransportBpp.runMock $ \cfg ->
          cfg & #statusWaitTimeSec .~ mockWaitTimeSeconds
            & hideLogging,
        PublicTransportSearchConsumer.runPublicTransportSearchConsumer $ \cfg ->
          cfg & hideLogging
            & #kafkaConsumerCfgs . #publicTransportSearch . #timeoutMilliseconds .~ kafkaConsumerTimeoutMilliseconds,
        SearchResultAggregator.runSearchResultAggregator $ \cfg ->
          cfg & hideLogging
            & #kafkaConsumerCfgs . #publicTransportQuotes . #timeoutMilliseconds .~ kafkaConsumerTimeoutMilliseconds
      ]

    startServers servers = do
      migrateDB
      prepareTestResources
      traverse_ forkIO servers
      -- Wait for servers to start up and migrations to run
      threadDelay 5e6

    cleanupServers _ = do
      releaseTestResources
      signalProcess sigINT =<< getProcessID

    migrateDB = do
      (appBackendCfg :: AppBackend.AppCfg) <- readDhallConfigDefault "app-backend"
      Esq.migrateIfNeeded appBackendCfg.migrationPath True appBackendCfg.esqDBCfg
        >>= handleLeft exitDBMigrationFailure "Couldn't migrate app-backend database: "

      (transportCfg :: TransporterBackend.AppCfg) <- readDhallConfigDefault "atlas-transport"
      Esq.migrateIfNeeded transportCfg.migrationPath True transportCfg.esqDBCfg
        >>= handleLeft exitDBMigrationFailure "Couldn't migrate atlas-transporter database: "

hideLogging :: HasField "loggerConfig" cfg LoggerConfig => cfg -> cfg
hideLogging cfg =
  cfg{loggerConfig =
        cfg.loggerConfig
          & #logToConsole .~ False
          & #logRawSql .~ False
          & #logToFile .~ True
     }
