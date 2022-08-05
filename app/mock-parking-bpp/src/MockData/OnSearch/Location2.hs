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

Module      :  MockData.OnSearch.Location2
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module MockData.OnSearch.Location2 where

import Beckn.Types.Core.Migration.Gps
import Core.OnSearch.Item
import Core.OnSearch.Location
import MockData.OnSearch.Common
import Relude hiding (id, state)

parkingLocationId :: Text
parkingLocationId = "P2"

mockLocation :: Location
mockLocation =
  let gps = mockGps
      id = parkingLocationId
      address = mockAddress "Test Address2"
   in Location {..}

mockGps :: Gps
mockGps =
  Gps
    { lat = 20.5437,
      lon = 78.9529
    }

--------------------------------------------

parkingItemId19 :: Text
parkingItemId19 = "19"

mockItem19 :: Item
mockItem19 = makeMockItem parkingItemId19 "Four wheeler parking for all" 12 parkingLocationId parkingPlacesAvailable4

parkingPlacesAvailable4 :: Int
parkingPlacesAvailable4 = 50
