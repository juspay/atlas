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

Module      :  Core.Context
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Core.Context where

import Beckn.Prelude
import Beckn.Types.Common
import Beckn.Types.Predicate
import Beckn.Utils.Validation
import qualified Core.Spec.Common.Context as Context
import qualified Core.Spec.Common.Domain as Domain

coreConfig :: CoreConfig
coreConfig =
  CoreConfig
    { version = "0.9.3",
      domain = Domain.PUBLIC_TRANSPORT,
      country = "IND",
      city = "Kochi"
    }

buildContext ::
  (MonadTime m, MonadGuid m) =>
  Context.Action ->
  Text ->
  Text ->
  BaseUrl ->
  Maybe Text ->
  Maybe BaseUrl ->
  m Context.Context
buildContext = buildContext' coreConfig

validateContext :: (MonadThrow m, Log m) => Context.Action -> Context.Context -> m ()
validateContext = validateContext' coreConfig

-- TODO We can move this common code to the lib

data CoreConfig = CoreConfig
  { version :: Text,
    domain :: Domain.Domain,
    country :: Text,
    city :: Text
  }

buildContext' ::
  (MonadTime m, MonadGuid m) =>
  CoreConfig ->
  Context.Action ->
  Text ->
  Text ->
  BaseUrl ->
  Maybe Text ->
  Maybe BaseUrl ->
  m Context.Context
buildContext' config action txnId bapId bapUri bppId bppUri = do
  timestamp <- getCurrentTime
  message_id <- generateGUIDText
  return
    Context.Context
      { domain = config.domain,
        country = config.country,
        city = config.city,
        action = action,
        core_version = config.version,
        bap_id = bapId,
        bap_uri = bapUri,
        bpp_id = bppId,
        bpp_uri = bppUri,
        transaction_id = Just txnId,
        message_id = message_id,
        timestamp = timestamp
      }

validateContext' :: (MonadThrow m, Log m) => CoreConfig -> Context.Action -> Context.Context -> m ()
validateContext' config action' = runRequestValidation validator
  where
    validator Context.Context {..} =
      sequenceA_
        [ validateField "domain" domain $ Exact config.domain,
          validateField "action" action $ Exact action',
          validateField "core_version" core_version $ Exact config.version,
          validateField "country" country $ Exact config.country
        ]
