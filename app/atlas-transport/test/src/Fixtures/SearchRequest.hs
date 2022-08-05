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

Module      :  Fixtures.SearchRequest
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Fixtures.SearchRequest (defaultSearchRequest) where

import Beckn.Types.Id
import qualified Domain.Types.SearchRequest as SearchRequest
import EulerHS.Prelude
import qualified Fixtures.Time as Fixtures
import Servant.Client

defaultSearchRequest :: SearchRequest.SearchRequest
defaultSearchRequest =
  SearchRequest.SearchRequest
    { id = Id "1",
      messageId = "",
      startTime = Fixtures.defaultTime,
      validTill = Fixtures.defaultTime,
      providerId = Id "",
      fromLocationId = "",
      toLocationId = Just "",
      bapId = "",
      bapUri = BaseUrl {baseUrlScheme = Http, baseUrlHost = "localhost", baseUrlPort = 8013, baseUrlPath = ""},
      createdAt = Fixtures.defaultTime
    }
