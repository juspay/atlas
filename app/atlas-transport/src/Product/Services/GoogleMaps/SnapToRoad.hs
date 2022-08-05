{-# LANGUAGE DerivingStrategies #-}


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

Module      :  Product.Services.GoogleMaps.SnapToRoad
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Product.Services.GoogleMaps.SnapToRoad where

import Beckn.Prelude
import Beckn.Types.App (MandatoryQueryParam, MonadFlow)
import Beckn.Types.Error (GenericError (InternalError))
import Beckn.Types.MapSearch
import Beckn.Utils.Common (callAPI, fromEitherM)
import qualified Data.Text as T
import EulerHS.Types as Euler
import Servant
import Servant.Client
import Tools.Metrics as Metrics

snapToRoadUrl :: BaseUrl
snapToRoadUrl =
  BaseUrl Https "roads.googleapis.com" 443 "v1/snapToRoads"

-- https://roads.googleapis.com/v1/snapToRoads
-- 443 port is for https

newtype PointsList = PointsList {unPointsList :: [LatLong]}
  deriving (Generic, Show)

convertPoint :: LatLong -> Text
convertPoint pt = mconcat [show pt.lat, ",", show pt.lon]

convertPointsList :: PointsList -> Text
convertPointsList = T.intercalate "|" . map convertPoint . unPointsList

newtype SnapToRoadResponse = SnapToRoadResponse
  { snappedPoints :: [SnappedPoint]
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

snappedPointToLatLongLine :: SnappedPoint -> Text
snappedPointToLatLongLine pt = mconcat [show pt.location.latitude, ",", show pt.location.longitude]

data SnappedPoint = SnappedPoint
  { location :: SPLocation,
    originalIndex :: Maybe Int,
    placeId :: Text
  }
  deriving (Generic, FromJSON, ToJSON)

data SPLocation = SPLocation
  { latitude :: Double,
    longitude :: Double
  }
  deriving (Generic, FromJSON, ToJSON)

snappedLocationtoLatLong :: SPLocation -> LatLong
snappedLocationtoLatLong s =
  LatLong
    { lat = s.latitude,
      lon = s.longitude
    }

latLongToSnappedLocation :: LatLong -> SPLocation
latLongToSnappedLocation ll =
  SPLocation
    { latitude = ll.lat,
      longitude = ll.lon
    }

type SnapToRoadAPI =
  MandatoryQueryParam "key" Text
    :> MandatoryQueryParam "interpolate" Bool
    :> MandatoryQueryParam "path" Text
    :> Get '[JSON] SnapToRoadResponse

callSnapToRoadAPI ::
  ( HasCallStack,
    Metrics.CoreMetrics m,
    MonadFlow m,
    MonadReader r m,
    HasField "googleMapsKey" r Text
  ) =>
  Bool ->
  PointsList ->
  m SnapToRoadResponse
callSnapToRoadAPI interpolate pointsList = do
  apiKey <- asks (.googleMapsKey)
  if apiKey == fakeApiKey
    then pure $ makeFakeResponse pointsList
    else do
      let eulerClient = Euler.client (Proxy @SnapToRoadAPI)
      callAPI snapToRoadUrl (eulerClient apiKey interpolate $ convertPointsList pointsList) "snap-to-road"
        >>= fromEitherM (\err -> InternalError $ "Failed to call snap-to-road API: " <> show err)

fakeApiKey :: Text
fakeApiKey = "mock-key"

makeFakeResponse :: PointsList -> SnapToRoadResponse
makeFakeResponse (PointsList pList) = SnapToRoadResponse $ map makeFakeSnappedPoint pList
  where
    makeFakeSnappedPoint latLong =
      SnappedPoint
        { location = latLongToSnappedLocation latLong,
          originalIndex = Nothing,
          placeId = ""
        }
