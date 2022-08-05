{-# LANGUAGE TypeApplications #-}


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
import Domain.Types.Quote
import Domain.Types.Quote.QuoteTerms
import Domain.Types.Quote.RentalQuote
import Domain.Types.SearchRequest
import Storage.Tabular.Quote

create :: Quote -> SqlDB ()
create quote = do
  create' quote
  traverse_ create' (mkQuoteTermsEntities quote)
  case quote.quoteDetails of
    OneWayDetails _ -> pure ()
    RentalDetails rentalDetails -> create' (mkRentalQuote quote.id rentalDetails)

findById :: Transactionable m => Id Quote -> m (Maybe Quote)
findById = Esq.findById

findByBPPQuoteId :: Transactionable m => Id BPPQuote -> m (Maybe Quote)
findByBPPQuoteId bppQuoteId_ =
  Esq.findOne $ do
    quote <- from $ table @QuoteT
    where_ $ quote ^. QuoteBppQuoteId ==. val (getId bppQuoteId_)
    return quote

findByTxnIdAndBppIdAndQuoteId :: Transactionable m => Id SearchRequest -> Text -> Id BPPQuote -> m (Maybe Quote)
findByTxnIdAndBppIdAndQuoteId txnId bppId quoteId =
  Esq.findOne $ do
    quote <- from $ table @QuoteT
    where_ $ quote ^. QuoteRequestId ==. val (toKey txnId) &&. quote ^. QuoteProviderId ==. val bppId &&. quote ^. QuoteBppQuoteId ==. val (getId quoteId)
    return quote

findAllByRequestId :: Transactionable m => Id SearchRequest -> m [Quote]
findAllByRequestId searchRequestId =
  Esq.findAll $ do
    quote <- from $ table @QuoteT
    where_ $ quote ^. QuoteRequestId ==. val (toKey searchRequestId)
    return quote
