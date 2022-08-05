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

Module      :  Core.Common.Payment
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Core.Common.Payment where

import Beckn.Prelude
import Beckn.Types.App ()
import Beckn.Utils.JSON
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Data.OpenApi (ToSchema (declareNamedSchema), fromAesonOptions, genericDeclareNamedSchema)

data PaymentStatus = PAID | NOT_PAID deriving (Generic, Eq, Show)

instance ToSchema PaymentStatus where
  declareNamedSchema = genericDeclareUnNamedSchema $ fromAesonOptions constructorsWithHyphens

instance FromJSON PaymentStatus where
  parseJSON = genericParseJSON constructorsWithHyphens

instance ToJSON PaymentStatus where
  toJSON = genericToJSON constructorsWithHyphens

data PaymentType = PRE_FULFILLMENT | POST_FULFILLMENT | ON_ORDER deriving (Generic, Show)

instance ToSchema PaymentType where
  declareNamedSchema = genericDeclareUnNamedSchema $ fromAesonOptions constructorsWithHyphens

instance FromJSON PaymentType where
  parseJSON = genericParseJSON constructorsWithHyphens

instance ToJSON PaymentType where
  toJSON = genericToJSON constructorsWithHyphens

data PaymentGatewayTransactionStatus = PAYMENT_LINK_CREATED | PAYMENT_LINK_EXPIRED | CAPTURED | REFUNDED deriving (Generic, Show)

instance ToSchema PaymentGatewayTransactionStatus where
  declareNamedSchema = genericDeclareUnNamedSchema $ fromAesonOptions constructorsToLowerOptions

instance FromJSON PaymentGatewayTransactionStatus where
  parseJSON = genericParseJSON constructorsToLowerOptions

instance ToJSON PaymentGatewayTransactionStatus where
  toJSON = genericToJSON constructorsToLowerOptions

data PaymentParams = PaymentParams
  { amount :: Text,
    currency :: Text,
    transaction_status :: PaymentGatewayTransactionStatus,
    transaction_id :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema PaymentParams where
  declareNamedSchema = genericDeclareUnNamedSchema $ fromAesonOptions stripPrefixUnderscoreIfAny

data Payment = Payment
  { params :: PaymentParams,
    _type :: PaymentType,
    status :: PaymentStatus,
    uri :: BaseUrl,
    tl_method :: Text
  }
  deriving (Generic, Show)

instance ToSchema Payment where
  declareNamedSchema = genericDeclareNamedSchema $ fromAesonOptions stripPrefixUnderscoreIfAny

instance FromJSON Payment where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Payment where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny
