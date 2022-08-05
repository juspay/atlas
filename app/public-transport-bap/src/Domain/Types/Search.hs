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

Module      :  Domain.Types.Search
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Domain.Types.Search where

import Beckn.Prelude
import Beckn.Types.Id
import Tools.Auth

data Search = Search
  { id :: Id Search,
    lat :: Double,
    lon :: Double,
    requestorId :: Id Person,
    createdAt :: UTCTime
  }
  deriving (Generic)

data SearchAPIEntity = SearchAPIEntity
  { id :: Id Search,
    lat :: Double,
    lon :: Double,
    requestorId :: Id Person
  }
  deriving (Generic, ToJSON)

makeSearchAPIEntity :: Search -> SearchAPIEntity
makeSearchAPIEntity Search {..} = do
  SearchAPIEntity
    { ..
    }
