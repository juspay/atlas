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

Module      :  Core.OnSearch.ItemQuantity
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Core.OnSearch.ItemQuantity where

import Beckn.Prelude
import Beckn.Types.Core.Migration.Scalar
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Data.OpenApi (ToSchema (declareNamedSchema), defaultSchemaOptions)

data Quantity = Quantity
  { count :: Int,
    measure :: Maybe Scalar
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema Quantity where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

newtype ItemQuantity = ItemQuantity
  { available :: Quantity
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema ItemQuantity where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
