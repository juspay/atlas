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

Module      :  Domain.Types.Quote
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Domain.Types.Quote where

import Beckn.Types.Amount
import Beckn.Types.Id
import Data.Time
import qualified Domain.Types.FareProduct as DFareProduct
import qualified Domain.Types.Organization as DOrg
import Domain.Types.Products (Products)
import qualified Domain.Types.RentalFarePolicy as DRentalFP
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.Vehicle as DVeh
import EulerHS.Prelude hiding (id)
import GHC.Records.Extra

data Quote = Quote
  { id :: Id Quote,
    requestId :: Id DSR.SearchRequest,
    productId :: Id Products, -- do we need this field?
    estimatedFare :: Amount,
    discount :: Maybe Amount,
    estimatedTotalFare :: Amount,
    providerId :: Id DOrg.Organization,
    vehicleVariant :: DVeh.Variant,
    createdAt :: UTCTime,
    quoteDetails :: QuoteDetails
  }

data QuoteDetails = OneWayDetails OneWayQuoteDetails | RentalDetails RentalQuoteDetails

data OneWayQuoteDetails = OneWayQuoteDetails
  { distance :: Double,
    distanceToNearestDriver :: Double
  }

data RentalQuoteDetails = RentalQuoteDetails
  { rentalFarePolicyId :: Id DRentalFP.RentalFarePolicy,
    baseDistance :: Double,
    baseDurationHr :: Int,
    descriptions :: [Text]
  }

getFareProductType :: QuoteDetails -> DFareProduct.FareProductType
getFareProductType = \case
  OneWayDetails _ -> DFareProduct.ONE_WAY
  RentalDetails _ -> DFareProduct.RENTAL

mkRentalQuoteDetails :: DRentalFP.RentalFarePolicy -> QuoteDetails
mkRentalQuoteDetails rentalFarePolicy@DRentalFP.RentalFarePolicy {..} =
  RentalDetails $
    RentalQuoteDetails
      { descriptions = mkDescriptions rentalFarePolicy,
        rentalFarePolicyId = id,
        ..
      }

mkDescriptions :: DRentalFP.RentalFarePolicy -> [Text]
mkDescriptions DRentalFP.RentalFarePolicy {..} =
  [ "Extra km fare: " <> show extraKmFare,
    "Extra min fare: " <> show extraMinuteFare,
    "Extra fare for day: " <> maybe "not allowed" show driverAllowanceForDay,
    "A rider can choose this package for a trip where the rider may not have a pre-decided destination and may not want to return to the origin location",
    "The rider may want to stop at multiple destinations and have the taxi wait for the rider at these locations"
  ]
