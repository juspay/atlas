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

Module      :  ExternalAPI.Flow
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module ExternalAPI.Flow where

import Beckn.Prelude
import Beckn.Tools.Metrics.CoreMetrics
import Beckn.Types.App
import Beckn.Types.Core.ReqTypes
import Beckn.Utils.Common
import Beckn.Utils.Error.BaseError.HTTPError.BecknAPIError (IsBecknAPI)
import Beckn.Utils.Servant.SignatureAuth
import Core.Spec.API.Search as Search
import qualified Core.Spec.Search as Search
import GHC.Records.Extra

search ::
  ( MonadFlow m,
    MonadReader r m,
    CoreMetrics m,
    HasField "bapId" r Text,
    HasField "gatewayUrl" r BaseUrl
  ) =>
  BecknReq Search.SearchMessage ->
  m ()
search req = do
  url <- asks (.gatewayUrl)
  callBecknAPIWithSignature "search" Search.searchAPI url req

callBecknAPIWithSignature ::
  ( MonadFlow m,
    MonadReader r m,
    CoreMetrics m,
    IsBecknAPI api req res,
    HasField "bapId" r Text
  ) =>
  Text ->
  Proxy api ->
  BaseUrl ->
  req ->
  m ()
callBecknAPIWithSignature a b c d = do
  bapId <- asks (.bapId)
  void $ callBecknAPI (Just $ getHttpManagerKey bapId) Nothing a b c d
