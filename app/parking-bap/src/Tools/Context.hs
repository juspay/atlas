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

Module      :  Tools.Context
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Tools.Context where

import Beckn.Prelude
import qualified Beckn.Product.Validation.Context as Validation
import Beckn.Types.Common
import Beckn.Utils.Common
import Beckn.Utils.Servant.BaseUrl (showBaseUrlText)
import qualified Core.Common.Context as Context
import qualified Core.Common.Domain as Domain

buildContext ::
  (MonadTime m, MonadGuid m) =>
  Context.Action ->
  Text ->
  BaseUrl ->
  Maybe BaseUrl ->
  m Context.Context
buildContext action txnId bapUri bppUri = do
  timestamp <- getCurrentTime
  message_id <- generateGUIDText
  return
    Context.Context
      { domain = Domain.PARKING,
        country = "IND",
        city = "Kochi",
        action = action,
        core_version = "0.9.3",
        bap_id = showBaseUrlText bapUri, -- maybe selfId?
        bap_uri = bapUri,
        bpp_id = show <$> bppUri,
        bpp_uri = bppUri,
        transaction_id = Just txnId,
        message_id = message_id,
        timestamp = timestamp
      }

validateContext :: (HasFlowEnv m r ["coreVersion" ::: Text, "domainVersion" ::: Text]) => Context.Action -> Context.Context -> m ()
validateContext action context = do
  Validation.validateDomain Domain.PARKING context
  Validation.validateContextCommons action context
