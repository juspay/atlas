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

Module      :  Domain.Action.Beckn.Search
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Domain.Action.Beckn.Search where

import Beckn.Prelude
import Beckn.Product.MapSearch.GoogleMaps (HasCoordinates (getCoordinates))
import qualified Beckn.Product.MapSearch.GoogleMaps as GoogleMaps
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Amount
import Beckn.Types.Common
import Beckn.Types.Id
import qualified Beckn.Types.MapSearch as MapSearch
import Beckn.Utils.Common (logDebug)
import Data.Time.Clock (addUTCTime)
import qualified Domain.Types.Organization as DOrg
import qualified Domain.Types.SearchReqLocation as DLoc
import qualified Domain.Types.SearchRequest as DSearchReq
import Domain.Types.SearchRequestForDriver
import Environment
import Product.FareCalculator.Flow
import SharedLogic.DriverPool
import Storage.Queries.Person
import qualified Storage.Queries.SearchReqLocation as QLoc
import qualified Storage.Queries.SearchRequest as QSReq
import qualified Storage.Queries.SearchRequestForDriver as QSRD

data DSearchReq = DSearchReq
  { messageId :: Text,
    transactionId :: Maybe Text,
    bapId :: Text,
    bapUri :: BaseUrl,
    gatewayUri :: BaseUrl,
    pickupLocation :: DLoc.SearchReqLocationAPIEntity,
    pickupTime :: UTCTime,
    dropLocation :: DLoc.SearchReqLocationAPIEntity
  }

handler :: DOrg.Organization -> DSearchReq -> Flow ()
handler org sReq = do
  fromLocation <- buildSearchReqLocation sReq.pickupLocation
  toLocation <- buildSearchReqLocation sReq.dropLocation
  searchReq <- buildSearchRequest fromLocation.id toLocation.id org.id sReq
  driverPool <- calculateDriverPool (getCoordinates fromLocation) org.id

  distance <-
    (.info.distance) <$> GoogleMaps.getDistance (Just MapSearch.CAR) (getCoordinates fromLocation) (getCoordinates toLocation) Nothing

  estimatedFare <- calculateFare org.id distance sReq.pickupTime
  logDebug $
    "search request id=" <> show searchReq.id
      <> "; estimated distance = "
      <> show distance
      <> "; estimated fare:"
      <> show estimatedFare
  searchRequestsForDrivers <- mapM (buildSearchRequestForDriver searchReq estimatedFare) driverPool
  Esq.runTransaction $ do
    QLoc.create fromLocation
    QLoc.create toLocation
    QSReq.create searchReq
    mapM_ QSRD.create searchRequestsForDrivers
  where
    buildSearchRequestForDriver ::
      (MonadFlow m) =>
      DSearchReq.SearchRequest ->
      FareParameters ->
      (DriverPoolResult, GoogleMaps.GetDistanceResultInfo) ->
      m SearchRequestForDriver
    buildSearchRequestForDriver searchRequest estFareParams (dpRes, gdRes) = do
      guid <- generateGUID
      now <- getCurrentTime
      pure
        SearchRequestForDriver
          { id = guid,
            searchRequestId = searchRequest.id,
            searchRequestValidTill = searchRequest.validTill,
            driverId = cast dpRes.driverId,
            vehicleVariant = dpRes.vehicle.variant,
            distanceToPickup = Meters $ floor gdRes.distance.getDistanceInMeter,
            durationToPickup = Seconds $ floor gdRes.duration,
            baseFare = amountToDouble $ fareSum estFareParams,
            createdAt = now,
            ..
          }

buildSearchRequest ::
  ( MonadTime m,
    MonadGuid m,
    MonadReader r m,
    HasField "searchRequestExpirationSeconds" r Int
  ) =>
  Id DLoc.SearchReqLocation ->
  Id DLoc.SearchReqLocation ->
  Id DOrg.Organization ->
  DSearchReq ->
  m DSearchReq.SearchRequest
buildSearchRequest fromId toId orgId sReq = do
  id_ <- Id <$> generateGUID
  createdAt_ <- getCurrentTime
  searchRequestExpirationSeconds <- asks (.searchRequestExpirationSeconds)
  let validTill_ = fromIntegral searchRequestExpirationSeconds `addUTCTime` createdAt_
  pure
    DSearchReq.SearchRequest
      { id = id_,
        transactionId = fromMaybe "" sReq.transactionId,
        messageId = sReq.messageId,
        validTill = validTill_,
        providerId = orgId,
        fromLocationId = fromId,
        toLocationId = toId,
        bapId = sReq.bapId,
        bapUri = sReq.bapUri,
        gatewayUri = sReq.gatewayUri,
        createdAt = createdAt_
      }

buildSearchReqLocation :: (MonadGuid m, MonadTime m) => DLoc.SearchReqLocationAPIEntity -> m DLoc.SearchReqLocation
buildSearchReqLocation DLoc.SearchReqLocationAPIEntity {..} = do
  id <- Id <$> generateGUID
  now <- getCurrentTime
  let createdAt = now
      updatedAt = now
  pure DLoc.SearchReqLocation {..}
