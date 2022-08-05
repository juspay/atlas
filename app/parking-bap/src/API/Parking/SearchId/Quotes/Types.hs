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

Module      :  API.Parking.SearchId.Quotes.Types
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module API.Parking.SearchId.Quotes.Types where

import Beckn.Prelude
import Beckn.Types.Id
import qualified Domain.Quote as DQuote
import qualified Domain.Search as DSearch
import Servant
import Tools.Auth

type API =
  Capture "searchId" (Id DSearch.Search)
    :> TokenAuth
    :> "quotes"
    :> Get '[JSON] GetQuotesRes

newtype GetQuotesRes = GetQuotesRes
  { quotes :: [DQuote.QuoteAPIEntity]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, ToSchema)
