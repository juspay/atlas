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
  ( runSearchResultAggregator,
  )
where

import Beckn.Exit
import Beckn.Prelude
import Beckn.Types.Flow (runFlowR)
import Beckn.Utils.App
import Beckn.Utils.Dhall (readDhallConfigDefault)
import Beckn.Utils.Servant.Server (runHealthCheckServerWithService)
import Environment
import Servant
import qualified Service.Runner as Runner

runSearchResultAggregator :: (AppCfg -> AppCfg) -> IO ()
runSearchResultAggregator configModifier = do
  appCfg <- configModifier <$> readDhallConfigDefault "search-result-aggregator"
  appEnv <-
    try (buildAppEnv appCfg)
      >>= handleLeftIO @SomeException exitBuildingAppEnvFailure "Couldn't build AppEnv: "

  runHealthCheckServerWithService appEnv identity identity EmptyContext (runService appEnv) releaseAppEnv pure
  where
    runService appEnv flowRt =
      runFlowR flowRt appEnv Runner.run
