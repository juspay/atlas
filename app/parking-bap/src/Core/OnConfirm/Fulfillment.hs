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

Module      :  Core.OnConfirm.Fulfillment
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Core.OnConfirm.Fulfillment where

import Beckn.Prelude
import Beckn.Utils.JSON (stripPrefixUnderscoreIfAny)
import Core.Common.Descriptor
import Core.Common.Gps
import Core.Common.Time
import Core.Common.Vehicle (Vehicle)
import Data.OpenApi (ToSchema (..), fromAesonOptions, genericDeclareNamedSchema)

data Fulfillment = Fulfillment
  { _type :: Text,
    tracking :: Bool,
    start :: Start,
    end :: End,
    vehicle :: Vehicle
  }
  deriving (Generic, Show)

instance ToSchema Fulfillment where
  declareNamedSchema = genericDeclareNamedSchema $ fromAesonOptions stripPrefixUnderscoreIfAny

instance FromJSON Fulfillment where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Fulfillment where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data Start = Start
  { location :: StartLocation,
    contact :: Contact,
    time :: Time
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

data StartLocation = StartLocation
  { id :: Text,
    descriptor :: Descriptor,
    gps :: Gps
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

newtype End = End
  { time :: Time
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data Contact = Contact
  { phone :: Text,
    email :: Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)
