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

Module      :  Resources
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Resources where

import qualified "app-backend" App.Types as BecknApp
import qualified "atlas-transport" App.Types as BecknTransport
import Beckn.Utils.Dhall (readDhallConfig)
import EulerHS.Prelude
import GHC.IO (unsafePerformIO)

{-# NOINLINE transporterAppEnv #-}
transporterAppEnv :: BecknTransport.AppEnv
transporterAppEnv = unsafePerformIO $ do
  appCfg <- readDhallConfig "../dhall-configs/dev/atlas-transport.dhall"
  let updLogCfg =
        appCfg.loggerConfig{logToFile = False,
                            logToConsole = False
                           }
      updAppCfg = appCfg{loggerConfig = updLogCfg}
  BecknTransport.buildAppEnv updAppCfg

{-# NOINLINE appBackendEnv #-}
appBackendEnv :: BecknApp.AppEnv
appBackendEnv = unsafePerformIO $ do
  appCfg <- readDhallConfig "../dhall-configs/dev/app-backend.dhall"
  let updLogCfg =
        appCfg.loggerConfig{logToFile = False,
                            logToConsole = False
                           }
      updAppCfg = appCfg{loggerConfig = updLogCfg}
  BecknApp.buildAppEnv updAppCfg

prepareTestResources :: IO ()
prepareTestResources =
  return $
    transporterAppEnv
      `seq` appBackendEnv
      `seq` ()

releaseTestResources :: IO ()
releaseTestResources = do
  BecknApp.releaseAppEnv appBackendEnv
  BecknTransport.releaseAppEnv transporterAppEnv
