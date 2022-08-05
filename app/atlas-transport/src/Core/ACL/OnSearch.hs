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

Module      :  Core.ACL.OnSearch
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Core.ACL.OnSearch (mkOnSearchMessage) where

import qualified Beckn.Types.Core.Taxi.OnSearch as OnSearch
import qualified Domain.Action.Beckn.Search as DSearch
import qualified Domain.Types.Quote as DQuote
import EulerHS.Prelude hiding (id, state)

mkOnSearchMessage ::
  DSearch.DOnSearchReq ->
  OnSearch.OnSearchMessage
mkOnSearchMessage DSearch.DOnSearchReq {..} = do
  let provider =
        OnSearch.Provider
          { id = transporterInfo.shortId.getShortId,
            name = transporterInfo.name,
            category_id = show fareProductType,
            items = map mkItem quotes,
            contacts = transporterInfo.contacts,
            rides_inprogress = transporterInfo.ridesInProgress,
            rides_completed = transporterInfo.ridesCompleted,
            rides_confirmed = transporterInfo.ridesConfirmed
          }
  OnSearch.OnSearchMessage $ OnSearch.Catalog [provider]

mkItem :: DQuote.Quote -> OnSearch.Item
mkItem DQuote.Quote {..} = do
  let (distanceToNearestDriver, baseDistance, baseDurationHr, descriptions) =
        case quoteDetails of
          DQuote.OneWayDetails details ->
            (Just details.distanceToNearestDriver, Nothing, Nothing, Nothing)
          DQuote.RentalDetails details ->
            (Nothing, Just details.baseDistance, Just details.baseDurationHr, Just details.descriptions)
  OnSearch.Item
    { id = id.getId,
      vehicle_variant = show vehicleVariant,
      estimated_price = OnSearch.Price $ realToFrac estimatedFare,
      discount = OnSearch.Price . realToFrac <$> discount,
      discounted_price = OnSearch.Price $ realToFrac estimatedTotalFare,
      nearest_driver_distance = realToFrac <$> distanceToNearestDriver,
      ..
    }
