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
import Domain.ParkingLocation
import Domain.Quote
import Domain.Search
import Storage.Tabular.ParkingLocation
import Storage.Tabular.Quote

findById :: Transactionable m => Id Quote -> m (Maybe Quote)
findById = Esq.findById

create :: Quote -> SqlDB ()
create = create'

findAllBySearchId :: Transactionable m => Id Search -> m [Quote]
findAllBySearchId searchId =
  Esq.findAll $ do
    quote <- from $ table @QuoteT
    where_ $ quote ^. QuoteSearchId ==. val (toKey searchId)
    return quote

findAllAggregatesBySearchId :: Transactionable m => Id Search -> m [(Quote, ParkingLocation)]
findAllAggregatesBySearchId searchId =
  findAll $ do
    (quote :& location) <-
      from $
        table @QuoteT
          `innerJoin` table @ParkingLocationT
            `Esq.on` ( \(quote :& location) ->
                         quote ^. QuoteParkingLocationId ==. location ^. ParkingLocationTId
                     )
    where_ $ quote ^. QuoteSearchId ==. val (toKey searchId)
    return (quote, location)
