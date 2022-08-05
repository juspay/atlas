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

Module      :  Core.OnConfirm.Order
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Core.OnConfirm.Order where

import Beckn.Prelude
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Core.Common.Billing
import Core.Common.Payment
import Core.Common.Price
import Core.OnConfirm.Fulfillment
import Core.OnConfirm.Item
import Core.OnConfirm.Provider
import Data.OpenApi (ToSchema (declareNamedSchema), defaultSchemaOptions)

data Order = Order
  { id :: Text,
    state :: Maybe OrderState,
    provider :: Provider,
    provider_location :: OrderProviderLocation,
    items :: [Item],
    billing :: Billing,
    fulfillment :: Fulfillment,
    quote :: SpecQuote,
    payment :: Payment
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

data OrderState = ACTIVE | CANCELLED | COMPLETE
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema OrderState where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

newtype OrderProviderLocation = OrderProviderLocation
  { id :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

instance ToSchema OrderProviderLocation where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

data SpecQuote = SpecQuote
  { price :: Price,
    breakup :: [Breakup]
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

data Breakup = Breakup
  { title :: Text,
    price :: Price
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)
