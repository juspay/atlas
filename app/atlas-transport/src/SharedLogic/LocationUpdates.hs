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

Module      :  SharedLogic.LocationUpdates

Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module SharedLogic.LocationUpdates
  ( RideInterpolationHandler (..),
    processWaypoints,
    recalcDistanceBatches,
    defaultRideInterpolationHandler,
    --- next functions are needed for tests:
    addPointsImplementation,
    getWaypointsNumberImplementation,
    getFirstNwaypointsImplementation,
    deleteFirstNwaypointsImplementation,
  )
where

import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Storage.Hedis (HedisFlow)
import Beckn.Storage.Hedis.Queries
import Beckn.Types.Common
import Beckn.Types.Id (Id)
import Beckn.Types.MapSearch
import Beckn.Utils.Logging
import qualified Domain.Types.Person as Person
import EulerHS.Prelude hiding (id, state)
import GHC.Records.Extra
import Product.Services.GoogleMaps.SnapToRoad (PointsList (PointsList), callSnapToRoadAPI, snappedLocationtoLatLong)
import SharedLogic.CalculateDistance
import qualified Storage.Queries.Ride as QRide
import Tools.Metrics as Metrics

data RideInterpolationHandler m = RideInterpolationHandler
  { batchSize :: Integer,
    addPoints :: Id Person.Person -> NonEmpty LatLong -> m (),
    getWaypointsNumber :: Id Person.Person -> m Integer,
    getFirstNwaypoints :: Id Person.Person -> Integer -> m [LatLong],
    deleteFirstNwaypoints :: Id Person.Person -> Integer -> m (),
    interpolatePoints :: [LatLong] -> m [LatLong],
    updateDistance :: Id Person.Person -> Double -> m ()
  }

--------------------------------------------------------------------------------
processWaypoints ::
  (Monad m, Log m) =>
  RideInterpolationHandler m ->
  Id Person.Person ->
  NonEmpty LatLong ->
  m ()
processWaypoints ih@RideInterpolationHandler {..} driverId waypoints = do
  addPoints driverId waypoints
  let ending = False
  recalcDistanceBatches ih ending driverId

recalcDistanceBatches ::
  (Monad m, Log m) =>
  RideInterpolationHandler m ->
  Bool ->
  Id Person.Person ->
  m ()
recalcDistanceBatches h@RideInterpolationHandler {..} ending driverId = do
  distanceToUpdate <- recalcDistanceBatches' 0
  updateDistance driverId distanceToUpdate
  where
    atLeastBatchPlusOne = getWaypointsNumber driverId <&> (> batchSize)
    pointsRemaining = (> 0) <$> getWaypointsNumber driverId
    recalcDistanceBatches' acc = do
      let continueCondition =
            if ending
              then pointsRemaining
              else atLeastBatchPlusOne
      batchLeft <- continueCondition
      if batchLeft
        then do
          dist <- recalcDistanceBatchStep h driverId
          recalcDistanceBatches' (acc + dist)
        else pure acc

recalcDistanceBatchStep ::
  (Monad m, Log m) =>
  RideInterpolationHandler m ->
  Id Person.Person ->
  m Double
recalcDistanceBatchStep RideInterpolationHandler {..} driverId = do
  batchWaypoints <- getFirstNwaypoints driverId (batchSize + 1)
  interpolatedWps <- interpolatePoints batchWaypoints
  let distance = getRouteLinearLength interpolatedWps
  logInfo $ mconcat ["calculated distance for ", show (length interpolatedWps), " points, ", "distance is ", show distance]
  deleteFirstNwaypoints driverId batchSize
  pure distance

-------------------------------------------------------------------------
defaultRideInterpolationHandler ::
  ( HedisFlow m env,
    HasPrettyLogger m env,
    HasCallStack,
    Metrics.CoreMetrics m,
    MonadFlow m,
    MonadReader env m,
    HasField "googleMapsKey" env Text,
    EsqDBFlow m env
  ) =>
  RideInterpolationHandler m
defaultRideInterpolationHandler =
  RideInterpolationHandler
    { batchSize = 98,
      addPoints = addPointsImplementation,
      getWaypointsNumber = getWaypointsNumberImplementation,
      getFirstNwaypoints = getFirstNwaypointsImplementation,
      deleteFirstNwaypoints = deleteFirstNwaypointsImplementation,
      interpolatePoints = callSnapToRoad,
      updateDistance = \driverId dist -> Esq.runTransaction $ QRide.updateDistance driverId dist
    }

makeWaypointsRedisKey :: Id Person.Person -> Text
makeWaypointsRedisKey driverId = mconcat ["waypoints", ":", driverId.getId]

addPointsImplementation :: (HedisFlow m env) => Id Person.Person -> NonEmpty LatLong -> m ()
addPointsImplementation driverId waypoints = do
  let key = makeWaypointsRedisKey driverId
      pointsList = toList waypoints :: [LatLong]
      numPoints = length pointsList
  rPush key pointsList
  logInfo $ mconcat ["added ", show numPoints, " points for riderId = ", driverId.getId]

getWaypointsNumberImplementation :: (HedisFlow m env) => Id Person.Person -> m Integer
getWaypointsNumberImplementation = lLen . makeWaypointsRedisKey

getFirstNwaypointsImplementation :: (HedisFlow m env) => Id Person.Person -> Integer -> m [LatLong]
getFirstNwaypointsImplementation driverId num = lRange (makeWaypointsRedisKey driverId) 0 (num - 1)

deleteFirstNwaypointsImplementation :: (HedisFlow m env) => Id Person.Person -> Integer -> m ()
deleteFirstNwaypointsImplementation driverId numToDel = lTrim (makeWaypointsRedisKey driverId) numToDel (-1)

callSnapToRoad ::
  ( HasCallStack,
    Metrics.CoreMetrics m,
    MonadFlow m,
    MonadReader r m,
    HasField "googleMapsKey" r Text
  ) =>
  [LatLong] ->
  m [LatLong]
callSnapToRoad wps = do
  res <- callSnapToRoadAPI True (PointsList wps)
  pure $ map (snappedLocationtoLatLong . (.location)) (res.snappedPoints)
