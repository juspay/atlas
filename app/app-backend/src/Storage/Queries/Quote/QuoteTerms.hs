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

Module      :  Storage.Queries.Quote.QuoteTerms
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Storage.Queries.Quote.QuoteTerms where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.Quote
import Domain.Types.Quote.QuoteTerms
import Storage.Tabular.Quote.QuoteTerms

findAllByQuoteId :: Transactionable m => Id Quote -> m [QuoteTermsEntity]
findAllByQuoteId quoteId =
  Esq.findAll $ do
    quoteTerms <- from $ table @QuoteTermsT
    where_ $ quoteTerms ^. QuoteTermsQuoteId ==. val (getId quoteId)
    return quoteTerms
