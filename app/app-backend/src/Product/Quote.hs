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

Module      :  Product.Quote
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Product.Quote where

import App.Types
import Beckn.Storage.Hedis as Hedis
import Beckn.Streaming.Kafka.Topic.PublicTransportQuoteList
import Beckn.Types.Id
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Quote as SQuote
import qualified Domain.Types.SearchReqLocation as Location
import qualified Domain.Types.SearchRequest as SSR
import EulerHS.Prelude hiding (id)
import qualified Product.MetroOffer as Metro
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.Quote as QRentalQuote
import qualified Storage.Queries.SearchReqLocation as Location
import qualified Storage.Queries.SearchRequest as QSR
import Types.API.MetroOffer (MetroOffer (..))
import qualified Types.API.Quote as API
import Types.Error
import Utils.Common

getQuotes :: Id SSR.SearchRequest -> Id Person.Person -> FlowHandler API.GetQuotesRes
getQuotes searchRequestId _ = withFlowHandlerAPI $ do
  searchRequest <- QSR.findById searchRequestId >>= fromMaybeM (SearchRequestDoesNotExist searchRequestId.getId)
  fromLocation <- Location.findById searchRequest.fromLocationId >>= fromMaybeM LocationNotFound
  mbToLocation <- forM (searchRequest.toLocationId) (Location.findById >=> fromMaybeM LocationNotFound)
  offers <- getOffers searchRequest
  return $
    API.GetQuotesRes
      { fromLocation = Location.makeSearchReqLocationAPIEntity fromLocation,
        toLocation = Location.makeSearchReqLocationAPIEntity <$> mbToLocation,
        quotes = offers
      }

getOffers :: (HedisFlow m r, EsqDBFlow m r) => SSR.SearchRequest -> m [API.OfferRes]
getOffers searchRequest = do
  -- ONE_WAY and RENTAL cases
  case searchRequest.toLocationId of
    Just _ -> do
      quoteList <- QQuote.findAllByRequestId searchRequest.id
      let quotes = API.OnDemandCab . SQuote.makeQuoteAPIEntity <$> sortByNearestDriverDistance quoteList
      metroOffers <- map API.Metro <$> Metro.getMetroOffers searchRequest.id
      publicTransportOffers <- map API.PublicTransport <$> getPubTransportOffers searchRequest.id
      return . sortBy (compare `on` creationTime) $ quotes <> metroOffers <> publicTransportOffers
    Nothing -> do
      quoteList <- QRentalQuote.findAllByRequestId searchRequest.id
      let quotes = API.OnDemandCab . SQuote.makeQuoteAPIEntity <$> sortByEstimatedFare quoteList
      return . sortBy (compare `on` creationTime) $ quotes
  where
    sortByNearestDriverDistance quoteList = do
      let sortFunc = compare `on` getMbDistanceToNearestDriver
      sortBy sortFunc quoteList
    getMbDistanceToNearestDriver quote = do
      case quote.quoteDetails of
        SQuote.OneWayDetails details -> Just details.distanceToNearestDriver
        SQuote.RentalDetails _ -> Nothing
    sortByEstimatedFare quoteList = do
      let sortFunc = compare `on` (.estimatedFare)
      sortBy sortFunc quoteList
    creationTime :: API.OfferRes -> UTCTime
    creationTime (API.OnDemandCab SQuote.QuoteAPIEntity {createdAt}) = createdAt
    creationTime (API.Metro MetroOffer {createdAt}) = createdAt
    creationTime (API.PublicTransport PublicTransportQuote {createdAt}) = createdAt

getPubTransportOffers :: (HedisFlow m r, MonadFlow m) => Id SSR.SearchRequest -> m [PublicTransportQuote]
getPubTransportOffers transactionId =
  Hedis.getList redisKey
  where
    redisKey = "publicTransportQuoteList:" <> getId transactionId
