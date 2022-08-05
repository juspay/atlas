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

Module      :  Domain.Types.FareProduct
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Domain.Types.FareProduct where

import Beckn.Prelude
import Beckn.Types.Id (Id)
import Beckn.Utils.JSON
import Data.OpenApi
import qualified Domain.Types.Organization as DOrg

data FareProductType = ONE_WAY | RENTAL deriving (Generic, Show, Read, Eq, FromJSON, ToJSON, ToSchema)

data FareProduct = FareProduct
  { id :: Id FareProduct,
    organizationId :: Id DOrg.Organization,
    _type :: FareProductType,
    createdAt :: UTCTime
  }
  deriving (Generic, Show, Eq)

data FareProductAPIEntity = FareProductAPIEntity
  { id :: Id FareProduct,
    _type :: FareProductType,
    createdAt :: UTCTime
  }
  deriving (Show, Generic)

instance FromJSON FareProductAPIEntity where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON FareProductAPIEntity where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance ToSchema FareProductAPIEntity where
  declareNamedSchema = genericDeclareNamedSchema $ fromAesonOptions stripPrefixUnderscoreIfAny

makeFareProductAPIEntity :: FareProduct -> FareProductAPIEntity
makeFareProductAPIEntity FareProduct {..} = FareProductAPIEntity {..}
