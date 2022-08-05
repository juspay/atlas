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

Module      :  Product.RentalFareCalculator.Calculator
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Product.RentalFareCalculator.Calculator where

import Beckn.Types.Amount (Amount (..))
import Beckn.Types.Common
import Data.Time
  ( LocalTime (localDay),
    TimeZone,
    diffDays,
    minutesToTimeZone,
    utcToLocalTime,
  )
import Domain.Types.RentalFarePolicy (RentalFarePolicy)
import qualified Domain.Types.RentalFarePolicy as DRentalFP
import EulerHS.Prelude
import Utils.Common

type TripStartTime = UTCTime

type TripStopTime = UTCTime

data RentalFareParameters = RentalFareParameters
  { baseFare :: Amount,
    extraDistanceFare :: Amount,
    extraTimeFare :: Amount,
    nextDaysFare :: Maybe Amount, --use 0 instead of Nothing?
    discount :: Maybe Amount
  }
  deriving stock (Show, Eq)

rentalFareSum :: RentalFareParameters -> Amount
rentalFareSum RentalFareParameters {..} = do
  let nextDaysFare' = fromMaybe 0 nextDaysFare
  baseFare + extraDistanceFare + extraTimeFare + nextDaysFare'

rentalFareSumWithDiscount :: RentalFareParameters -> Amount
rentalFareSumWithDiscount fp@RentalFareParameters {..} = do
  let fareSumm = rentalFareSum fp
  max 0 $ maybe fareSumm (fareSumm -) discount

calculateRentalFareParameters ::
  DRentalFP.RentalFarePolicy ->
  Meter ->
  TripStartTime ->
  TripStopTime ->
  RentalFareParameters
calculateRentalFareParameters farePolicy distance startTime stopTime = do
  let baseFare = farePolicy.baseFare
      extraDistanceFare = calculateExtraDistanceFare farePolicy distance
      extraTimeFare = calculateExtraTimeFare farePolicy startTime stopTime
      nextDaysFare = calculateNextDaysFare farePolicy startTime stopTime
      discount = Nothing -- rental discount is not implemented yet
  RentalFareParameters {..}

calculateExtraDistanceFare ::
  RentalFarePolicy ->
  Meter ->
  Amount
calculateExtraDistanceFare farePolicy distance = do
  let distanceInKm = getDistanceInMeter distance / 1000
  let extraDistance = distanceInKm - farePolicy.baseDistance
  if extraDistance > 0
    then Amount (toRational extraDistance) * farePolicy.extraKmFare
    else 0

calculateExtraTimeFare ::
  RentalFarePolicy ->
  TripStartTime ->
  TripStopTime ->
  Amount
calculateExtraTimeFare farePolicy tripStartTime tripStopTime = do
  let tripTime = diffUTCTime tripStopTime tripStartTime
      tripTimeInMinutes = nominalDiffTimeToSeconds tripTime `div` 60
      extraTime = toInteger tripTimeInMinutes - toInteger farePolicy.baseDurationHr * 60
  if extraTime > 0
    then Amount (toRational extraTime) * farePolicy.extraMinuteFare
    else 0

calculateNextDaysFare ::
  RentalFarePolicy ->
  TripStartTime ->
  TripStopTime ->
  Maybe Amount
calculateNextDaysFare farePolicy tripStartTime tripStopTime = do
  let tripDays = calcTripDays tripStartTime tripStopTime
  if tripDays > 0
    then calcNextDaysFare tripDays <$> farePolicy.driverAllowanceForDay
    else Nothing
  where
    calcNextDaysFare :: Integer -> Amount -> Amount
    calcNextDaysFare tripDays nextDaysFare = Amount (toRational tripDays) * nextDaysFare

timeZoneIST :: TimeZone
timeZoneIST = minutesToTimeZone 330 -- TODO: Should be configurable. Hardcoded to IST +0530

-- Calculate how many days the trip lasts.
-- Every day is added at midnight local time
calcTripDays :: TripStartTime -> TripStopTime -> Integer
calcTripDays tripStartTime tripStopTime = diffDays (calcDay tripStopTime) (calcDay tripStartTime)
  where
    calcDay = localDay . utcToLocalTime timeZoneIST
