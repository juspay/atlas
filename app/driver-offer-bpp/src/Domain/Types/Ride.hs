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
import Beckn.Types.Id
import qualified Domain.Types.DriverQuote as DQuote
import qualified Domain.Types.Organization as DOrg
import Domain.Types.Person
import qualified Domain.Types.SearchReqLocation as DLoc
import qualified Domain.Types.SearchRequest as DSR

data RideStatus = Active | Inactive

data Ride = Ride
  { id :: Id Ride,
    transactionId :: Text,
    requestId :: Id DSR.SearchRequest,
    quoteId :: Id DQuote.DriverQuote,
    status :: RideStatus,
    driverId :: Id Person,
    providerId :: Id DOrg.Organization,
    bapId :: Text,
    bapUri :: BaseUrl,
    startTime :: UTCTime,
    otp :: Text,
    fromLocationId :: Id DLoc.SearchReqLocation,
    baseFare :: Double,
    extraFareSelected :: Maybe Double,
    traveledDistance :: Double,
    chargeableDistance :: Maybe Double,
    trackingUrl :: Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
