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

Module      :  Core.Spec.Search.Fulfillment
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Core.Spec.Search.Fulfillment where

import Beckn.Prelude
import Beckn.Utils.GenericPretty
import Beckn.Utils.Schema
import Core.Spec.Search.LocationGps
import Data.OpenApi (ToSchema (declareNamedSchema), defaultSchemaOptions)

data Fulfillment = Fulfillment
  { start :: StartInfo,
    end :: EndInfo
  }
  deriving (Generic, Show, ToJSON, FromJSON, PrettyShow)

instance ToSchema Fulfillment where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

data StartInfo = StartInfo
  { location :: LocationGps,
    time :: StartTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, PrettyShow)

instance ToSchema StartInfo where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

newtype StartTime = StartTime {range :: TimeRange}
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON, PrettyShow)

instance ToSchema StartTime where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

data TimeRange = TimeRange
  { start :: UTCTime,
    end :: UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, PrettyShow)

instance ToSchema TimeRange where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

newtype EndInfo = EndInfo
  { location :: LocationGps
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON, PrettyShow)

instance ToSchema EndInfo where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
