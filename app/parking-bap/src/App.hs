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

Module      :  App

Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module App
  ( runService,
  )
where

import API.Handler
import API.Types
import App.Types
import Beckn.Exit
import Beckn.Prelude
import Beckn.Storage.Esqueleto.Migration (migrateIfNeeded)
import Beckn.Storage.Redis.Config (prepareRedisConnections)
import Beckn.Types.Flow (FlowR)
import Beckn.Utils.App
import Beckn.Utils.Dhall (readDhallConfigDefault)
import Beckn.Utils.Servant.Server (runServerWithHealthCheck)
import Beckn.Utils.Servant.SignatureAuth (modFlowRtWithAuthManagers)
import Servant (Context (..))
import Tools.Auth

runService :: (AppCfg -> AppCfg) -> IO ()
runService configModifier = do
  appCfg <- readDhallConfigDefault "parking-bap" <&> configModifier
  appEnv <- buildAppEnv appCfg
  runServerWithHealthCheck appEnv (Proxy @API) handler middleware identity context releaseAppEnv \flowRt -> do
    try (prepareRedisConnections $ appCfg.redisCfg)
      >>= handleLeft @SomeException exitRedisConnPrepFailure "Exception thrown: "
    migrateIfNeeded appCfg.migrationPath appCfg.autoMigrate appCfg.esqDBCfg
      >>= handleLeft exitDBMigrationFailure "Couldn't migrate database: "
    modFlowRtWithAuthManagers flowRt appEnv [(appCfg.selfId, appCfg.authEntity.uniqueKeyId)]
  where
    middleware =
      hashBodyForSignature
        >>> supportProxyAuthorization
    context = verifyPersonAction @(FlowR AppEnv) :. EmptyContext
