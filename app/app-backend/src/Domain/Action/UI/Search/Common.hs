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

Module      :  Domain.Action.UI.Search.Common
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Domain.Action.UI.Search.Common where

import Beckn.Types.Id
import qualified Domain.Types.SearchReqLocation as Location
import qualified Domain.Types.SearchRequest as DSearchReq
import qualified Domain.Types.SearchRequest as SearchRequest
import EulerHS.Prelude hiding (state)
import Tools.Metrics (CoreMetrics)
import qualified Types.API.Search as API
import Utils.Common

buildSearchRequest ::
  ( (HasFlowEnv m r ["searchRequestExpiry" ::: Maybe Seconds, "graphhopperUrl" ::: BaseUrl]),
    MonadFlow m,
    CoreMetrics m
  ) =>
  Text ->
  Location.SearchReqLocation ->
  Maybe Location.SearchReqLocation ->
  Maybe Double ->
  UTCTime ->
  m SearchRequest.SearchRequest
buildSearchRequest userId pickup mbDrop mbDistance now = do
  searchRequestId <- generateGUID
  validTill <- getSearchRequestExpiry now
  return
    SearchRequest.SearchRequest
      { id = searchRequestId,
        startTime = now,
        validTill = validTill,
        riderId = Id userId,
        fromLocationId = pickup.id,
        toLocationId = mbDrop <&> (.id),
        distance = mbDistance,
        createdAt = now
      }
  where
    getSearchRequestExpiry :: (HasFlowEnv m r '["searchRequestExpiry" ::: Maybe Seconds]) => UTCTime -> m UTCTime
    getSearchRequestExpiry startTime = do
      searchRequestExpiry <- maybe 7200 fromIntegral <$> asks (.searchRequestExpiry)
      let minExpiry = 300 -- 5 minutes
          timeToRide = startTime `diffUTCTime` now
          validTill = addUTCTime (minimum [fromInteger searchRequestExpiry, maximum [minExpiry, timeToRide]]) now
      pure validTill

buildSearchReqLoc :: MonadFlow m => API.SearchReqLocation -> m Location.SearchReqLocation
buildSearchReqLoc API.SearchReqLocation {..} = do
  now <- getCurrentTime
  locId <- generateGUID
  return
    Location.SearchReqLocation
      { id = locId,
        lat = gps.lat,
        lon = gps.lon,
        city = address.city,
        state = address.state,
        country = address.country,
        street = address.street,
        door = address.door,
        building = address.building,
        areaCode = address.areaCode,
        area = address.area,
        createdAt = now,
        updatedAt = now
      }
