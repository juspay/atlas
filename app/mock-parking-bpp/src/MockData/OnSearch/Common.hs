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

Module      :  MockData.OnSearch.Common
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module MockData.OnSearch.Common where

import Core.OnSearch.Address
import Core.OnSearch.Category
import Core.OnSearch.Item
import Core.OnSearch.ItemQuantity
import Relude hiding (id, state)
import Utils (buildPrice)

cat4wheeler :: Text
cat4wheeler = "4-wheeler-parking"

mockProviderCategories :: Category
mockProviderCategories =
  let id_ = Just cat4wheeler
      descriptor_ = Just $ CategoryDescriptor {name = "4 wheeler parking"}
   in Category
        { id = id_,
          descriptor = descriptor_
        }

mockAddress :: Text -> Address
mockAddress name =
  let street_address = "mock street address"
      locality = "Panangad"
      city = Nothing
      state = "Kerala"
      country = "IND"
      area_code = "682506"
   in Address {..}

makeMockItem :: Text -> Text -> Int -> Text -> Int -> Item
makeMockItem _id description rupeesPrice locationId availablePlaces =
  let id = _id
      descriptor =
        ItemDescriptor
          { name = description,
            images = []
          }
      price = buildPrice rupeesPrice
      category_id = cat4wheeler
      location_id = locationId
      matched = True
      quantity =
        ItemQuantity
          { available =
              Quantity
                { count = availablePlaces,
                  measure = Nothing
                }
          }
   in Item {..}
