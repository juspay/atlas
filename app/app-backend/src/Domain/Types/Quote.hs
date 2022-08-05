{-# LANGUAGE UndecidableInstances #-}


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

Module      :  Domain.Types.Quote
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Domain.Types.Quote where

import Beckn.Prelude
import Beckn.Types.Amount
import Beckn.Types.Id
import Data.Aeson
import Data.OpenApi (ToSchema (..), genericDeclareNamedSchema)
import qualified Data.OpenApi as OpenApi
import qualified Domain.Types.SearchRequest as DSearchRequest

data FareProductType = ONE_WAY | RENTAL deriving (Generic, Show, Read, Eq, FromJSON, ToJSON, ToSchema)

data BPPQuote

data Quote = Quote
  { id :: Id Quote,
    bppQuoteId :: Id BPPQuote,
    requestId :: Id DSearchRequest.SearchRequest,
    estimatedFare :: Amount,
    discount :: Maybe Amount,
    estimatedTotalFare :: Amount,
    providerId :: Text,
    providerUrl :: BaseUrl,
    providerName :: Text,
    providerMobileNumber :: Text,
    providerCompletedRidesCount :: Int,
    vehicleVariant :: Text,
    createdAt :: UTCTime,
    quoteTerms :: [QuoteTerms],
    quoteDetails :: QuoteDetails
  }
  deriving (Generic, Show)

data QuoteDetails = OneWayDetails OneWayQuoteDetails | RentalDetails RentalQuoteDetails
  deriving (Show, Generic)

instance ToJSON QuoteDetails where
  toJSON = genericToJSON fareProductOptions

instance FromJSON QuoteDetails where
  parseJSON = genericParseJSON fareProductOptions

instance ToSchema QuoteDetails where
  declareNamedSchema = genericDeclareNamedSchema fareProductSchemaOptions

fareProductOptions :: Options
fareProductOptions =
  defaultOptions
    { sumEncoding = fareProductTaggedObject,
      constructorTagModifier = fareProductConstructorModifier
    }

fareProductSchemaOptions :: OpenApi.SchemaOptions
fareProductSchemaOptions =
  OpenApi.defaultSchemaOptions
    { OpenApi.sumEncoding = fareProductTaggedObject,
      OpenApi.constructorTagModifier = fareProductConstructorModifier
    }

fareProductTaggedObject :: SumEncoding
fareProductTaggedObject =
  defaultTaggedObject
    { tagFieldName = "fareProductType"
    }

fareProductConstructorModifier :: String -> String
fareProductConstructorModifier = \case
  "OneWayDetails" -> "ONE_WAY"
  "RentalDetails" -> "RENTAL"
  x -> x

-- Can I use distanceToNearestDriver instead of nearestDriverDistance in QuoteAPIEntity for consistency and less boilerplate?
newtype OneWayQuoteDetails = OneWayQuoteDetails
  { distanceToNearestDriver :: Double
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data RentalQuoteDetails = RentalQuoteDetails
  { baseDistance :: Double,
    baseDurationHr :: Int
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data QuoteTerms = QuoteTerms
  { id :: Id QuoteTerms,
    description :: Text
  }
  deriving (Show)

data QuoteAPIEntity = QuoteAPIEntity
  { id :: Id Quote,
    vehicleVariant :: Text,
    estimatedFare :: Amount,
    estimatedTotalFare :: Amount,
    discount :: Maybe Amount,
    agencyName :: Text,
    agencyNumber :: Text,
    agencyCompletedRidesCount :: Int,
    descriptions :: [Text],
    quoteDetails :: QuoteDetails,
    createdAt :: UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

makeQuoteAPIEntity :: Quote -> QuoteAPIEntity
makeQuoteAPIEntity Quote {..} = do
  QuoteAPIEntity
    { agencyName = providerName,
      agencyNumber = providerMobileNumber,
      agencyCompletedRidesCount = providerCompletedRidesCount,
      descriptions = quoteTerms <&> (.description),
      ..
    }
