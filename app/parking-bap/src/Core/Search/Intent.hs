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

Module      :  Core.Search.Intent
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Core.Search.Intent where

import Beckn.Prelude
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Core.Common.Time (Time)
import Core.Search.Location
import Data.OpenApi (ToSchema (declareNamedSchema), defaultSchemaOptions)

newtype Intent = Intent
  { fulfillment :: FulFillmentInfo
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data FulFillmentInfo = FulFillmentInfo
  { start :: TimeInfo,
    end :: LocationAndTime
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema FulFillmentInfo where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

newtype TimeInfo = TimeInfo
  { time :: Time
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

instance ToSchema TimeInfo where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

data LocationAndTime = LocationAndTime
  { location :: Location,
    time :: Time
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema LocationAndTime where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
