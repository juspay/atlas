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

Module      :  Core.Spec.Search.LocationGps
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Core.Spec.Search.LocationGps where

import Beckn.Types.Core.Migration.Gps
import Beckn.Utils.GenericPretty (PrettyShow)
import Beckn.Utils.Schema
import Data.Aeson
import Data.OpenApi (ToSchema (declareNamedSchema), defaultSchemaOptions)
import Relude hiding (id)

newtype LocationGps = LocationGps {gps :: Gps}
  deriving (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, PrettyShow)

instance ToSchema LocationGps where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
