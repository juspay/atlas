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

Module      :  Types.API.Quote
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Types.API.Quote where

import Beckn.Streaming.Kafka.Topic.PublicTransportQuoteList (PublicTransportQuote)
import Beckn.Utils.JSON (objectWithSingleFieldParsing)
import qualified Beckn.Utils.Schema as S
import Data.Char (toLower)
import Data.OpenApi (ToSchema (..), genericDeclareNamedSchema)
import Domain.Types.Quote (QuoteAPIEntity)
import Domain.Types.SearchReqLocation (SearchReqLocationAPIEntity)
import EulerHS.Prelude hiding (id)
import Types.API.MetroOffer

data GetQuotesRes = GetQuotesRes
  { fromLocation :: SearchReqLocationAPIEntity,
    toLocation :: Maybe SearchReqLocationAPIEntity,
    quotes :: [OfferRes]
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data OfferRes
  = OnDemandCab QuoteAPIEntity
  | Metro MetroOffer
  | PublicTransport PublicTransportQuote
  deriving (Show, Generic)

instance ToJSON OfferRes where
  toJSON = genericToJSON $ objectWithSingleFieldParsing \(f : rest) -> toLower f : rest

instance FromJSON OfferRes where
  parseJSON = genericParseJSON $ objectWithSingleFieldParsing \(f : rest) -> toLower f : rest

instance ToSchema OfferRes where
  declareNamedSchema = genericDeclareNamedSchema $ S.objectWithSingleFieldParsing \(f : rest) -> toLower f : rest
