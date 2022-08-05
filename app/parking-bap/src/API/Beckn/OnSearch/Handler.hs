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

Module      :  API.Beckn.OnSearch.Handler
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module API.Beckn.OnSearch.Handler where

import App.Types
import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Common hiding (id)
import Beckn.Types.Core.ReqTypes
import Beckn.Types.Error
import Beckn.Types.Id
import Beckn.Utils.Common
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult)
import qualified Core.Common.Context as Context
import qualified Core.Common.DecimalValue as DecimalValue
import qualified Core.OnSearch as OnSearch
import qualified Domain.ParkingLocation as DParkingLocation
import qualified Domain.Quote as DQuote
import qualified Domain.Search as DSearch
import qualified Storage.Queries.ParkingLocation as QParkingLocation
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.Search as QSearch
import Tools.Context (validateContext)
import qualified Tools.Metrics as Metrics

handler ::
  SignatureAuthResult ->
  SignatureAuthResult ->
  BecknCallbackReq OnSearch.OnSearchCatalog ->
  FlowHandler AckResponse
handler _ _ req = withFlowHandlerBecknAPI . withTransactionIdLogTag req $ do
  logTagDebug "on_search req" (encodeToText req)
  validateContext Context.ON_SEARCH $ req.context
  transactionId <- req.context.transaction_id & fromMaybeM (InvalidRequest "Context.transaction_id is not present.")
  Metrics.finishSearchMetrics transactionId
  case req.contents of
    Right msg -> searchCbService req transactionId msg.catalog
    Left err -> logTagError "on_search req" $ "on_search error: " <> show err
  return Ack

searchCbService :: EsqDBFlow m r => BecknCallbackReq OnSearch.OnSearchCatalog -> Text -> OnSearch.Catalog -> m ()
searchCbService req transactionId catalog = do
  let searchRequestId = Id transactionId
  _searchRequest <- QSearch.findById searchRequestId >>= fromMaybeM (SearchRequestDoesNotExist searchRequestId.getId)
  bppUrl <- req.context.bpp_uri & fromMaybeM (InvalidRequest "Missing bpp_url")
  bppId <- req.context.bpp_id & fromMaybeM (InvalidRequest "Missing bpp_id")
  let providers = catalog.bpp_providers
  when (null providers) $ throwError $ InvalidRequest "Missing bpp_provider"
  now <- getCurrentTime
  parkingLocations <- do
    allParkingLocationsByProvider <- forM providers $ \provider -> do
      forM provider.locations (buildParkingLocation now)
    return $ concat allParkingLocationsByProvider
  quotes <- do
    allQuotesByProvider <- forM providers $ \provider -> do
      let items = fromMaybe [] provider.items
      forM items (buildQuote now searchRequestId bppUrl bppId parkingLocations)
    return $ concat allQuotesByProvider
  Esq.runTransaction $ do
    traverse_ QParkingLocation.create parkingLocations
    traverse_ QQuote.create quotes

buildQuote ::
  MonadFlow m =>
  UTCTime ->
  Id DSearch.Search ->
  BaseUrl ->
  Text ->
  [DParkingLocation.ParkingLocation] ->
  OnSearch.Item ->
  m DQuote.Quote
buildQuote now searchId bppUrl bppId parkingLocations item = do
  let parkingSpaceName = item.descriptor.name
  let availableSpaces = item.quantity.available.count
  let bppItemId = item.id
  fare <-
    DecimalValue.convertDecimalValueToAmount item.price.value
      & fromMaybeM (InvalidRequest "Unable to parse price")
  parkingLocation <-
    find (\pl -> pl.idFromBpp == item.location_id) parkingLocations
      & fromMaybeM (InvalidRequest "Invalid item.location_id")
  quoteId <- generateGUID
  return
    DQuote.Quote
      { id = quoteId,
        parkingLocationId = parkingLocation.id,
        parkingLocationIdFromBpp = item.location_id,
        createdAt = now,
        ..
      }

buildParkingLocation :: MonadGuid m => UTCTime -> OnSearch.Location -> m DParkingLocation.ParkingLocation
buildParkingLocation now location = do
  id <- generateGUID
  return
    DParkingLocation.ParkingLocation
      { id = Id id,
        idFromBpp = location.id,
        lat = location.gps.lat,
        lon = location.gps.lon,
        name = location.address.name,
        country = location.address.country,
        city = location.address.city,
        state = location.address.state,
        locality = location.address.locality,
        areaCode = location.address.area_code,
        streetAddress = location.address.street_address,
        createdAt = now
      }
