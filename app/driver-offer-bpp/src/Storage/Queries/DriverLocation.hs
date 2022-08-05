{-# LANGUAGE NamedWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}


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

Module      :  Storage.Queries.DriverLocation
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Storage.Queries.DriverLocation where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Beckn.Types.MapSearch (LatLong (..))
import Domain.Types.DriverLocation
import Domain.Types.Person
import Storage.Tabular.DriverLocation

create :: Id Person -> LatLong -> UTCTime -> SqlDB ()
create drLocationId latLong updateTime =
  -- Tricky query to be able to insert meaningful Point
  Esq.insertSelect' $
    return $
      DriverLocationT
        <# val (toKey drLocationId)
        <#> val latLong.lat
        <#> val latLong.lon
        <#> Esq.getPoint (val latLong.lat, val latLong.lon)
        <#> val updateTime
        <#> val updateTime

findById ::
  Transactionable m =>
  Id Person ->
  m (Maybe DriverLocation)
findById = Esq.findById

upsertGpsCoord :: Id Person -> LatLong -> UTCTime -> SqlDB ()
upsertGpsCoord drLocationId latLong updateTime = do
  mbDrLoc <- Esq.findById @_ @DriverLocation drLocationId
  case mbDrLoc of
    Nothing -> Storage.Queries.DriverLocation.create drLocationId latLong updateTime
    Just _ -> Esq.update' $ \tbl -> do
      set
        tbl
        [ DriverLocationLat =. val latLong.lat,
          DriverLocationLon =. val latLong.lon,
          DriverLocationUpdatedAt =. val updateTime,
          DriverLocationPoint =. Esq.getPoint (val latLong.lat, val latLong.lon)
        ]
      where_ $ tbl ^. DriverLocationTId ==. val (toKey $ cast drLocationId)
