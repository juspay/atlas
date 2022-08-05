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

module Core.ACL.OnSearch where

import Beckn.Prelude
import qualified Beckn.Types.Core.Taxi.OnSearch as OnSearch
import Beckn.Types.Id (ShortId)
import qualified Domain.Types.DriverQuote as DQuote
import qualified Domain.Types.Organization as DOrg

data DOnSearchReq = DOnSearchReq
  { transporterInfo :: TransporterInfo,
    quotes :: [DQuote.DriverQuote]
  }

data TransporterInfo = TransporterInfo
  { shortId :: ShortId DOrg.Organization,
    name :: Text,
    contacts :: Text,
    ridesInProgress :: Int,
    ridesCompleted :: Int,
    ridesConfirmed :: Int
  }

mkOnSearchMessage ::
  DOnSearchReq ->
  OnSearch.OnSearchMessage
mkOnSearchMessage DOnSearchReq {..} = do
  let provider =
        OnSearch.Provider
          { id = transporterInfo.shortId.getShortId,
            name = transporterInfo.name,
            category_id = "ONE_WAY",
            items = map mkItem quotes,
            contacts = transporterInfo.contacts,
            rides_inprogress = transporterInfo.ridesInProgress,
            rides_completed = transporterInfo.ridesCompleted,
            rides_confirmed = transporterInfo.ridesConfirmed
          }
  OnSearch.OnSearchMessage $ OnSearch.Catalog [provider]

mkItem :: DQuote.DriverQuote -> OnSearch.Item
mkItem q =
  OnSearch.Item
    { id = q.id.getId,
      vehicle_variant = show q.vehicleVariant,
      estimated_price = estimated_price_,
      discount = Nothing,
      discounted_price = estimated_price_,
      nearest_driver_distance = Just $ OnSearch.DecimalValue $ toRational q.distanceToPickup.getMeters,
      baseDistance = Nothing,
      baseDurationHr = Nothing,
      descriptions = Nothing
    }
  where
    estimated_price_ = OnSearch.Price $ realToFrac $ q.baseFare + fromMaybe 0 q.extraFareSelected
