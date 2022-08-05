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

Module      :  SharedLogic.DriverPool

Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module SharedLogic.DriverPool
  ( calculateDriverPool,
  )
where

import Beckn.External.GoogleMaps.Types (HasGoogleMaps)
import qualified Beckn.Product.MapSearch.GoogleMaps as GoogleMaps
import Beckn.Storage.Esqueleto (Transactionable)
import Beckn.Types.Id
import Beckn.Types.MapSearch
import qualified Beckn.Types.MapSearch as GoogleMaps
import qualified Domain.Types.Organization as SOrg
import EulerHS.Prelude hiding (id)
import qualified Storage.Queries.Person as QP
import Tools.Metrics
import Utils.Common

calculateDriverPool ::
  ( Transactionable m,
    HasFlowEnv m r ["defaultRadiusOfSearch" ::: Meters, "driverPositionInfoExpiry" ::: Maybe Seconds],
    CoreMetrics m,
    HasGoogleMaps m r
  ) =>
  LatLong ->
  Id SOrg.Organization ->
  m [(QP.DriverPoolResult, GoogleMaps.GetDistanceResultInfo)]
calculateDriverPool pickupLatLong orgId = do
  radius <- fromIntegral <$> asks (.defaultRadiusOfSearch)
  approxDriverPool <-
    measuringDurationToLog INFO "calculateDriverPool" $
      QP.getNearestDrivers
        pickupLatLong
        radius
        orgId
  case approxDriverPool of
    [] -> pure []
    (a : pprox) -> filterOutDriversWithDistanceAboveThreshold radius pickupLatLong (a :| pprox)

filterOutDriversWithDistanceAboveThreshold ::
  ( Transactionable m,
    CoreMetrics m,
    MonadFlow m,
    HasGoogleMaps m r
  ) =>
  Integer ->
  LatLong ->
  NonEmpty QP.DriverPoolResult ->
  m [(QP.DriverPoolResult, GoogleMaps.GetDistanceResultInfo)]
filterOutDriversWithDistanceAboveThreshold threshold pickupLatLong driverPoolResults = do
  getDistanceResults <- GoogleMaps.getDistancesGeneral (Just GoogleMaps.CAR) driverPoolResults (pickupLatLong :| []) zipFunc Nothing
  pure $ filter (filterFunc . snd) getDistanceResults
  where
    zipFunc dpRes _ estDist = (dpRes, estDist)
    filterFunc estDist = getDistanceInMeter estDist.distance <= fromIntegral threshold
