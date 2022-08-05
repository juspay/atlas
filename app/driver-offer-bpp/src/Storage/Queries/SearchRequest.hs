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

Module      :  Storage.Queries.SearchRequest
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Storage.Queries.SearchRequest where

import Beckn.Prelude
import Beckn.Product.MapSearch.GoogleMaps
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Beckn.Types.MapSearch
import Beckn.Utils.GenericPretty (PrettyShow)
import Domain.Types.Organization
import Domain.Types.SearchReqLocation
import Domain.Types.SearchRequest
import Storage.Tabular.SearchReqLocation
import Storage.Tabular.SearchRequest

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 func (a, b, c) = func a b c

create :: SearchRequest -> SqlDB ()
create = Esq.create'

findById :: Transactionable m => Id SearchRequest -> m (Maybe SearchRequest)
findById = Esq.findById

data NearbySearchRequestEntry = NearbySearchRequestEntry
  { searchRequest :: SearchRequest,
    fromLocation :: SearchReqLocation,
    straightDistanceMeters :: Double
  }
  deriving (Generic, PrettyShow, Show)

instance HasCoordinates NearbySearchRequestEntry where
  getCoordinates = getCoordinates . (.fromLocation)

findNearestByPoint :: (Transactionable m, Functor m) => Int -> Id Organization -> LatLong -> m [NearbySearchRequestEntry]
findNearestByPoint radiusMeters orgId driverPos =
  fmap (map $ uncurry3 NearbySearchRequestEntry) $
    Esq.findAll $ do
      sReq :& sFromLoc <- do
        from
          ( table @SearchRequestT
              `innerJoin` table @SearchReqLocationT `Esq.on` (\(s :& loc1) -> s ^. SearchRequestFromLocationId ==. loc1 ^. SearchReqLocationTId)
          )
      let dist = Esq.getPoint (val driverPos.lat, val driverPos.lon) <->. (sFromLoc ^. SearchReqLocationPoint)
      where_ $
        dist <=. val (fromIntegral radiusMeters)
          &&. sReq ^. SearchRequestProviderId ==. val (toKey orgId)
      pure (sReq, sFromLoc, dist)
