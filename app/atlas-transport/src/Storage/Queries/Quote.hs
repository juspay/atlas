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

Module      :  Storage.Queries.Quote
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Storage.Queries.Quote where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.Products
import Domain.Types.Quote
import Domain.Types.Quote.OneWayQuote
import Domain.Types.Quote.RentalQuote
import Domain.Types.SearchRequest
import Storage.Tabular.Quote

create :: Quote -> SqlDB ()
create quote = do
  create' quote
  case quote.quoteDetails of
    OneWayDetails oneWayDetails -> create' (mkOneWayQuote quote.id oneWayDetails)
    RentalDetails rentalDetails -> create' (mkRentalQuote quote.id rentalDetails)

findAllByProductIds :: Transactionable m => Integer -> Integer -> [Id Products] -> m [Quote]
findAllByProductIds limit_ offset_ ids = do
  findAll $ do
    quote <- from $ table @QuoteT
    where_ $ quote ^. QuoteProductId `in_` valList (toKey <$> ids)
    orderBy [desc $ quote ^. QuoteCreatedAt]
    limit $ fromIntegral limit_
    offset $ fromIntegral offset_
    return quote

findAllByRequestId :: Transactionable m => Id SearchRequest -> m [Quote]
findAllByRequestId requestId =
  findAll $ do
    quote <- from $ table @QuoteT
    where_ $ quote ^. QuoteRequestId ==. val (toKey requestId)
    return quote

findById :: Transactionable m => Id Quote -> m (Maybe Quote)
findById = Esq.findById
