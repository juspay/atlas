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

Module      :  Domain.Types.SavedReqLocation
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Domain.Types.SavedReqLocation where

import Beckn.Prelude
import Beckn.Types.Id
import Domain.Types.Person (Person)
import Domain.Types.SearchReqLocation (SearchReqLocationAPIEntity (..))

data SavedReqLocation = SavedReqLocation
  { id :: Id SavedReqLocation,
    lat :: Double,
    lon :: Double,
    street :: Maybe Text,
    door :: Maybe Text,
    city :: Maybe Text,
    state :: Maybe Text,
    country :: Maybe Text,
    building :: Maybe Text,
    areaCode :: Maybe Text,
    area :: Maybe Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    tag :: Text,
    riderId :: Id Person
  }
  deriving (Generic, Show)

data SavedReqLocationAPIEntity = SavedReqLocationAPIEntity
  { address :: SearchReqLocationAPIEntity,
    tag :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

makeSavedReqLocationAPIEntity :: SavedReqLocation -> SavedReqLocationAPIEntity
makeSavedReqLocationAPIEntity SavedReqLocation {..} =
  let address = SearchReqLocationAPIEntity {..}
   in SavedReqLocationAPIEntity
        { ..
        }
