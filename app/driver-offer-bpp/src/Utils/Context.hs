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

Module      :  Utils.Context
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Utils.Context where

import Beckn.Prelude
import Beckn.Types.Common
import Beckn.Types.Core.Context
import Data.String.Conversions
import Domain.Types.Organization (Organization (..))
import Servant.Client

defaultCountry :: Text
defaultCountry = "IND"

defaultCity :: Text
defaultCity = "Bangalore"

contextTemplate ::
  ( MonadTime m,
    MonadReader r m,
    HasField "coreVersion" r Text,
    HasField "nwAddress" r BaseUrl
  ) =>
  Organization ->
  Action ->
  Text ->
  BaseUrl ->
  Maybe Text ->
  Text ->
  m Context
contextTemplate org action bap_id bap_uri transaction_id message_id = do
  now <- getCurrentTime
  core_version <- asks (.coreVersion)
  nwAddress <- asks (.nwAddress)
  let bpp_id = org.shortId.getShortId
  let bpp_uri = nwAddress {baseUrlPath = baseUrlPath nwAddress <> "/" <> cs org.id.getId}
  pure
    Context
      { domain = MOBILITY,
        country = defaultCountry,
        city = defaultCity,
        timestamp = now,
        bpp_id = Just bpp_id,
        bpp_uri = Just bpp_uri,
        ..
      }
