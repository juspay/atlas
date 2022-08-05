{-# LANGUAGE UndecidableInstances #-}


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

Module      :  Domain.Types.Ride
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Domain.Types.Ride where

import Beckn.Prelude
import Beckn.Types.Amount
import Beckn.Types.Id
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import qualified Domain.Types.RideBooking as DRB
import Servant.API

data RideStatus
  = NEW
  | INPROGRESS
  | COMPLETED
  | CANCELLED
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

instance FromHttpApiData RideStatus where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = left T.pack . eitherDecode . BSL.fromStrict

instance ToHttpApiData RideStatus where
  toUrlPiece = DT.decodeUtf8 . toHeader
  toQueryParam = toUrlPiece
  toHeader = BSL.toStrict . encode

data BPPRide

data Ride = Ride
  { id :: Id Ride,
    bppRideId :: Id BPPRide,
    bookingId :: Id DRB.RideBooking,
    shortId :: ShortId Ride,
    status :: RideStatus,
    driverName :: Text,
    driverRating :: Maybe Double,
    driverMobileNumber :: Text,
    driverRegisteredAt :: UTCTime,
    vehicleNumber :: Text,
    vehicleModel :: Text,
    vehicleColor :: Text,
    vehicleVariant :: Text,
    otp :: Text,
    trackingUrl :: Text,
    fare :: Maybe Amount,
    totalFare :: Maybe Amount,
    chargeableDistance :: Maybe Double,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show)

data RideAPIEntity = RideAPIEntity
  { id :: Id Ride,
    shortRideId :: ShortId Ride,
    status :: RideStatus,
    driverName :: Text,
    driverNumber :: Text,
    driverRatings :: Maybe Double,
    driverRegisteredAt :: UTCTime,
    vehicleNumber :: Text,
    vehicleColor :: Text,
    vehicleVariant :: Text,
    vehicleModel :: Text,
    rideOtp :: Text,
    computedPrice :: Maybe Amount,
    chargeableRideDistance :: Maybe Double,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Show, FromJSON, ToJSON, Generic, ToSchema)

makeRideAPIEntity :: Ride -> RideAPIEntity
makeRideAPIEntity Ride {..} =
  RideAPIEntity
    { shortRideId = shortId,
      driverNumber = driverMobileNumber,
      driverRatings = driverRating,
      rideOtp = otp,
      computedPrice = totalFare,
      chargeableRideDistance = chargeableDistance,
      ..
    }
