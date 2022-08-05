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
import Beckn.Prelude
import Beckn.Utils.App
import Beckn.Utils.Dhall
import Beckn.Utils.Servant.Server (runServer)
import Beckn.Utils.Servant.SignatureAuth
import Environment
import Servant (Context (..))

runService :: (AppCfg -> AppCfg) -> IO ()
runService configModifier = do
  appEnv <- buildAppEnv . configModifier =<< readDhallConfigDefault "mock-bap"
  runServer appEnv (Proxy @API) handler middleware identity EmptyContext (const identity) releaseAppEnv $ \flowRt -> do
    modFlowRtWithAuthManagers flowRt appEnv [(appEnv.selfId, appEnv.authEntity.uniqueKeyId)]
  where
    middleware = hashBodyForSignature
