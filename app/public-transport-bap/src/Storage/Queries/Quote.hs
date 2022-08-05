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
import Domain.Types.Search
import Domain.Types.TransportStation
import Storage.Tabular.Quote
import Storage.Tabular.TransportStation

findById :: Transactionable m => Id Quote -> m (Maybe Quote)
findById quoteId =
  Esq.findOne $ do
    quote <- from $ table @QuoteT
    where_ $ quote ^. QuoteId ==. val (getId quoteId)
    return quote

create :: Quote -> SqlDB ()
create = create'

findAllBySearchId :: Transactionable m => Id Search -> m [Quote]
findAllBySearchId searchId =
  Esq.findAll $ do
    quote <- from $ table @QuoteT
    where_ $ quote ^. QuoteSearchId ==. val (toKey searchId)
    return quote

findAllAggregatesBySearchId :: Transactionable m => Id Search -> m [(Quote, TransportStation, TransportStation)]
findAllAggregatesBySearchId searchId =
  findAll $ do
    (quote :& depStation :& arrStation) <-
      from $
        table @QuoteT
          `innerJoin` table @TransportStationT
            `Esq.on` ( \(quote :& depStation) ->
                         quote ^. QuoteDepartureStationId ==. depStation ^. TransportStationTId
                     )
          `innerJoin` table @TransportStationT
            `Esq.on` ( \(quote :& _ :& arrStation) ->
                         quote ^. QuoteArrivalStationId ==. arrStation ^. TransportStationTId
                     )
    where_ $ quote ^. QuoteSearchId ==. val (toKey searchId)
    return (quote, depStation, arrStation)
