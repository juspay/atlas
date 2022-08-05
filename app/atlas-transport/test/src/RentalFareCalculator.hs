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

Module      :  RentalFareCalculator
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module RentalFareCalculator where

import Beckn.Types.Amount
import Beckn.Types.App
import Beckn.Types.Id
import Data.Time hiding (parseTime)
import qualified Domain.Types.Organization as Organization
import Domain.Types.RentalFarePolicy
import qualified Domain.Types.Vehicle as Vehicle
import EulerHS.Prelude
import Product.RentalFareCalculator
import Test.Hspec
import Test.Tasty
import Test.Tasty.HUnit
import Types.Error
import Utils.GuidGenerator ()
import Utils.SilentLogger ()
import Utils.Time

defaultFarePolicy :: RentalFarePolicy
defaultFarePolicy =
  RentalFarePolicy
    { id = rentalFarePolicyId,
      vehicleVariant = Vehicle.HATCHBACK,
      organizationId = orgID,
      baseFare = 120.0,
      baseDistance = 100,
      baseDurationHr = 3,
      extraKmFare = 2,
      extraMinuteFare = 1,
      driverAllowanceForDay = Just 30
    }

rentalFarePolicyId :: Id RentalFarePolicy
rentalFarePolicyId = "rentalFarePolicyId"

handle :: ServiceHandle IO
handle =
  ServiceHandle
    { getRentalFarePolicy = \rentalFarePolicyId_ -> pure $ case rentalFarePolicyId_ of
        "rentalFarePolicyId" -> Just defaultFarePolicy
        _ -> Nothing
    }

-- FIXME can we provide mock data without parsing?
mockTime :: Int -> Int -> UTCTime
mockTime hours minutes =
  parseTime $
    "2018-12-06T"
      <> (if hours <= 9 then "0" else "")
      <> show hours
      <> ":"
      <> (if minutes <= 9 then "0" else "")
      <> show minutes
      <> ":00.000Z"

mockTime2 :: UTCTime
mockTime2 = parseTime "2018-12-09T00:00:00.000Z"

orgID :: Id Organization.Organization
orgID = "organization_id"

-- Calculation tests

onlyBaseFare :: TestTree
onlyBaseFare = testCase "Rental fare consist of only base fare" $ do
  fareParams <- doCalculateRentalFare handle rentalFarePolicyId distance startTime stopTime
  let totalFare = rentalFareSumWithDiscount fareParams
  totalFare @?= Amount 120.0
  where
    startTime = mockTime 2 0
    stopTime = mockTime 4 30
    distance = Meter 90000.0

edgeCase :: TestTree
edgeCase = testCase "Edge case for rental fare" $ do
  fareParams <- doCalculateRentalFare handle rentalFarePolicyId distance startTime stopTime
  let totalFare = rentalFareSumWithDiscount fareParams
  totalFare @?= Amount 120.0
  where
    startTime = mockTime 18 30
    stopTime = mockTime 21 30
    distance = Meter 100000.0

incorrectData :: TestTree
incorrectData = testCase "Incorrect data for rental fare" $ do
  fareParams <- doCalculateRentalFare handle rentalFarePolicyId distance startTime stopTime
  let totalFare = rentalFareSumWithDiscount fareParams
  totalFare @?= Amount 120.0
  where
    startTime = mockTime 4 30
    stopTime = mockTime 2 0
    distance = Meter 90000.0

-- 120+7*2=134
extraDistance :: TestTree
extraDistance = testCase "Rental fare consist of base fare and extra distance fare" $ do
  fareParams <- doCalculateRentalFare handle rentalFarePolicyId distance startTime stopTime
  let totalFare = rentalFareSumWithDiscount fareParams
  totalFare @?= Amount 134.0
  where
    startTime = mockTime 2 0
    stopTime = mockTime 4 30
    distance = Meter 107000.0

-- 120+1*40=160
extraTime :: TestTree
extraTime = testCase "Rental fare consist of base fare and extra time fare" $ do
  fareParams <- doCalculateRentalFare handle rentalFarePolicyId distance startTime stopTime
  let totalFare = rentalFareSumWithDiscount fareParams
  totalFare @?= Amount 160.0
  where
    startTime = mockTime 2 0
    stopTime = mockTime 5 40
    distance = Meter 90000.0

-- using local time IST +0530, so this trip started at 23:30 and ended next day at 02:10
-- 120+1*30=150
nextDay :: TestTree
nextDay = testCase "Rental fare consist of base fare and next day fare" $ do
  fareParams <- doCalculateRentalFare handle rentalFarePolicyId distance startTime stopTime
  let totalFare = rentalFareSumWithDiscount fareParams
  totalFare @?= Amount 150.0
  where
    startTime = mockTime 18 0
    stopTime = mockTime 20 40
    distance = Meter 90000.0

-- base fare: 120; extra distance fare: 15*2=30; extra time fare : (3*24*60-3*60)*1=4140; next days fare: 30*3=90; total fare: 4380
allFareParameters :: TestTree
allFareParameters = testCase "Rental fare consist of all fare parameters" $ do
  fareParams <- doCalculateRentalFare handle rentalFarePolicyId distance startTime stopTime
  let totalFare = rentalFareSumWithDiscount fareParams
  totalFare @?= Amount 4380.0
  where
    startTime = mockTime 0 0
    stopTime = mockTime2
    distance = Meter 115000.0

failOnMissingFareConfig :: TestTree
failOnMissingFareConfig = testCase "Fail on missing RentalFarePolicy" $ do
  doCalculateRentalFare handle "fakeRentalFarePolicyId" distance startTime stopTime `shouldThrow` (== NoRentalFarePolicy)
  where
    startTime = mockTime 2 0
    stopTime = mockTime 4 30
    distance = Meter 90000.0

rentalFareCalculator :: TestTree
rentalFareCalculator =
  testGroup
    "Rental Fare Calculator"
    [ onlyBaseFare,
      edgeCase,
      incorrectData,
      extraDistance,
      extraTime,
      nextDay,
      allFareParameters,
      failOnMissingFareConfig
    ]
