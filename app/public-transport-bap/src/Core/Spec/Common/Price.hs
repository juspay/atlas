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

Module      :  Core.Spec.Common.Price
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Core.Spec.Common.Price where

import Beckn.Prelude
import Beckn.Types.Amount
import Beckn.Utils.GenericPretty
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Core.Spec.Common.DecimalValue
import Data.Aeson
import Data.OpenApi hiding (name, value)

data Price = Price
  { currency :: Text,
    value :: Amount
  }
  deriving (Generic, Show, PrettyShow)

instance ToSchema Price where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

instance FromJSON Price where
  parseJSON = withObject "price" $ \obj -> do
    currency <- obj .: "currency"
    decimalValue <- obj .: "value"
    value <- maybe (fail "invalid price value") pure $ convertDecimalValueToAmount decimalValue
    pure Price {..}

instance ToJSON Price where
  toJSON p =
    object
      [ "currency" .= p.currency,
        "value" .= convertAmountToDecimalValue (p.value)
      ]

rupeePrice :: Amount -> Price
rupeePrice = Price "INR"
