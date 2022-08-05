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

Module      :  MockData.OnSearch.Location1
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module MockData.OnSearch.Location1 where

import Beckn.Types.Core.Migration.Gps
import Core.OnSearch.Item
import Core.OnSearch.Location
import MockData.OnSearch.Common
import Relude hiding (id, state)

parkingLocationId :: Text
parkingLocationId = "P1"

mockLocation :: Location
mockLocation =
  let gps = mockGps
      id = parkingLocationId
      address = mockAddress "Test Address"
   in Location {..}

mockGps :: Gps
mockGps =
  Gps
    { lat = 20.5937,
      lon = 78.9629
    }

--------------------------------------------

parkingItemId4 :: Text
parkingItemId4 = "4"

mockItem4 :: Item
mockItem4 = makeMockItem parkingItemId4 "Four wheeler parking" 10 parkingLocationId parkingPlacesAvailable4

parkingPlacesAvailable4 :: Int
parkingPlacesAvailable4 = 14

-------------------------------------------------

parkingItemId9 :: Text
parkingItemId9 = "9"

mockItem9 :: Item
mockItem9 = makeMockItem parkingItemId9 "Four wheeler parking (luxury)" 20 parkingLocationId parkingPlacesAvailable9

parkingPlacesAvailable9 :: Int
parkingPlacesAvailable9 = 2
