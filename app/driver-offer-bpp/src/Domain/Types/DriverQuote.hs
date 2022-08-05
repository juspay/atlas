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

Module      :  Domain.Types.DriverQuote
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Domain.Types.DriverQuote where

import Beckn.Prelude
import Beckn.Types.Common
import Beckn.Types.Id
import Domain.Types.Person
import Domain.Types.SearchRequest
import qualified Domain.Types.Vehicle.Variant as Variant

data DriverQuoteStatus = Active | Inactive
  deriving (Show, Read)

data DriverQuote = DriverQuote
  { id :: Id DriverQuote,
    status :: DriverQuoteStatus,
    searchRequestId :: Id SearchRequest,
    driverId :: Id Person,
    baseFare :: Double,
    extraFareSelected :: Maybe Double,
    vehicleVariant :: Variant.Variant,
    distanceToPickup :: Meters,
    durationToPickup :: Seconds,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    validTill :: UTCTime
  }
