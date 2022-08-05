{-# OPTIONS_GHC -Wno-orphans #-}


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

Module      :  Domain.Types.Quote
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Domain.Types.Quote where

import Beckn.Prelude
import Beckn.Types.Amount
import Beckn.Types.Id
import Beckn.Utils.GenericPretty (PrettyShow)
import Domain.Types.Search
import Domain.Types.TransportStation

data Quote = Quote
  { id :: Id Quote,
    searchId :: Id Search,
    bppId :: Text,
    bppUrl :: BaseUrl,
    description :: Text,
    fare :: Amount,
    departureTime :: UTCTime,
    arrivalTime :: UTCTime,
    departureStationId :: Id TransportStation,
    arrivalStationId :: Id TransportStation,
    createdAt :: UTCTime,
    routeCode :: Text
  }
  deriving (Generic, Show, PrettyShow, ToJSON, FromJSON)

data QuoteAPIEntity = QuoteAPIEntity
  { id :: Id Quote,
    description :: Text,
    fare :: Amount,
    departureTime :: UTCTime,
    arrivalTime :: UTCTime,
    departureStation :: TransportStationAPIEntity,
    arrivalStation :: TransportStationAPIEntity,
    createdAt :: UTCTime
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, PrettyShow, Show)

makeQuoteAPIEntity :: Quote -> TransportStation -> TransportStation -> QuoteAPIEntity
makeQuoteAPIEntity Quote {..} departureStation arrivalStation = do
  let departureStationAPIEntity = makeTransportStationAPIEntity departureStation
      arrivalStationAPIEntity = makeTransportStationAPIEntity arrivalStation
  QuoteAPIEntity
    { departureStation = departureStationAPIEntity,
      arrivalStation = arrivalStationAPIEntity,
      ..
    }
