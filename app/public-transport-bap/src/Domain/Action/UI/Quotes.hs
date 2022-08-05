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

Module      :  Domain.Action.UI.Quotes
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Domain.Action.UI.Quotes where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import Beckn.Utils.GenericPretty (PrettyShow)
import qualified Domain.Types.Quote as DQuote
import qualified Domain.Types.Search as DSearch
import Storage.Queries.Quote as QQuote

newtype GetQuotesRes = GetQuotesRes
  { quotes :: [DQuote.QuoteAPIEntity]
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema, PrettyShow)

getQuotesHandler :: EsqDBFlow m r => Id DSearch.Search -> m GetQuotesRes
getQuotesHandler searchId = do
  quoteAggregates <- QQuote.findAllAggregatesBySearchId searchId
  let quoteAPIEntities = map makeQuoteAPIEntity quoteAggregates
  return $ GetQuotesRes quoteAPIEntities
  where
    makeQuoteAPIEntity (quote, depStation, arrStation) =
      DQuote.makeQuoteAPIEntity quote depStation arrStation
