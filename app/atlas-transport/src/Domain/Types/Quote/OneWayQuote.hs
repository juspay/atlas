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

Module      :  Domain.Types.Quote.OneWayQuote
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Domain.Types.Quote.OneWayQuote where

import Beckn.Types.Id
import qualified Domain.Types.Quote as DQuote
import EulerHS.Prelude hiding (id)

-- Not used in business logic, only for Tabular
data OneWayQuote = OneWayQuote
  { quoteId :: Id DQuote.Quote,
    distance :: Double,
    distanceToNearestDriver :: Double
  }
  deriving (Generic, Show, Eq)

mkOneWayQuote :: Id DQuote.Quote -> DQuote.OneWayQuoteDetails -> OneWayQuote
mkOneWayQuote quoteId DQuote.OneWayQuoteDetails {..} = OneWayQuote {..}
