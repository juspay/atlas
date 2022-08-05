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

Module      :  API.Parking.Search.Types
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module API.Parking.Search.Types where

import API.Types.Common (Gps)
import Beckn.Prelude
import Beckn.Types.Id
import qualified Domain.Search as DSearch
import Servant
import Tools.Auth

type API =
  "search"
    :> TokenAuth
    :> ReqBody '[JSON] SearchReq
    :> Post '[JSON] SearchRes

data SearchReq = SearchReq
  { location :: Gps,
    fromDate :: UTCTime,
    toDate :: UTCTime
  }
  deriving (Generic, FromJSON, ToSchema)

newtype SearchRes = SearchRes
  { searchId :: Id DSearch.Search
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, ToSchema)
