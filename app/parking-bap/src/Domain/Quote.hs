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

Module      :  Domain.Quote
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Domain.Quote where

import Beckn.Prelude
import Beckn.Types.Amount
import Beckn.Types.Id
import Domain.ParkingLocation
import Domain.Search

data Quote = Quote
  { id :: Id Quote,
    searchId :: Id Search,
    bppId :: Text,
    bppUrl :: BaseUrl,
    bppItemId :: Text,
    parkingSpaceName :: Text,
    parkingLocationId :: Id ParkingLocation,
    parkingLocationIdFromBpp :: Text,
    fare :: Amount,
    availableSpaces :: Int,
    createdAt :: UTCTime
  }
  deriving (Generic)

data QuoteAPIEntity = QuoteAPIEntity
  { id :: Id Quote,
    bppId :: Text,
    bppUrl :: BaseUrl,
    parkingSpaceName :: Text,
    parkingSpaceLocation :: ParkingLocationAPIEntity,
    fare :: Amount,
    availableSpaces :: Int,
    createdAt :: UTCTime
  }
  deriving (Generic, ToJSON, ToSchema)

makeQuoteAPIEntity :: Quote -> ParkingLocation -> QuoteAPIEntity
makeQuoteAPIEntity Quote {..} parkingLocation = do
  let parkingLocationAPIEntity = makeParkingLocationAPIEntity parkingLocation
  QuoteAPIEntity
    { parkingSpaceLocation = parkingLocationAPIEntity,
      ..
    }
