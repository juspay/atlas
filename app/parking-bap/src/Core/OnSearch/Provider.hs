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

Module      :  Core.OnSearch.Provider
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Core.OnSearch.Provider (Provider (..)) where

import Beckn.Prelude hiding (exp)
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Core.Common.Descriptor
import Core.OnSearch.Category (Category)
import Core.OnSearch.Item (Item)
import Core.OnSearch.Location (Location)
import Data.OpenApi (ToSchema (declareNamedSchema), defaultSchemaOptions)

data Provider = Provider
  { id :: Maybe Text,
    descriptor :: Descriptor,
    categories :: [Category],
    locations :: [Location],
    items :: Maybe [Item]
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema Provider where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
