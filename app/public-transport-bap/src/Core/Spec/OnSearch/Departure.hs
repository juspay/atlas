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

Module      :  Core.Spec.OnSearch.Departure
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Core.Spec.OnSearch.Departure where

import Beckn.Prelude
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Data.OpenApi

data Departure = Departure
  { id :: Text,
    route_id :: Text,
    start_time :: TimeStamp,
    end_time :: TimeStamp
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

instance ToSchema Departure where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

newtype TimeStamp = TimeStamp
  { timestamp :: UTCTime
  }
  deriving (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance ToSchema TimeStamp where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
