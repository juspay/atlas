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

Module      :  Fixtures.Quote
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Fixtures.Quote where

import Beckn.Types.Id
import qualified Domain.Types.Quote as Quote
import qualified Domain.Types.Vehicle as Veh
import EulerHS.Prelude
import qualified Fixtures.Time as Fixtures

defaultQuote :: Quote.Quote
defaultQuote = do
  let quoteDetails =
        Quote.OneWayQuoteDetails
          { distance = 0,
            distanceToNearestDriver = 0
          }
  Quote.Quote
    { id = Id "1",
      requestId = Id "1",
      productId = Id "1",
      estimatedFare = 0,
      estimatedTotalFare = 0,
      discount = Nothing,
      providerId = "",
      vehicleVariant = Veh.SUV,
      createdAt = Fixtures.defaultTime,
      quoteDetails = Quote.OneWayDetails quoteDetails
    }
