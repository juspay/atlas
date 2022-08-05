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

Module      :  Main
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Main where

import DistanceCalculation
import EulerHS.Prelude
import FareCalculator
import Flow.Allocation.AllocationTimeFinished
import Flow.Allocation.Cancellation
import Flow.Allocation.NotificationStatus
import Flow.Allocation.OnePoolTwoRide
import Flow.Allocation.Reassignment
import Flow.Allocation.TwoAllocations
import Flow.RideAPI.CancelRide (cancelRide)
import Flow.RideAPI.EndRide (endRideTests)
import Flow.RideAPI.StartRide (startRide)
import qualified LocationUpdates as LocUpd
import RentalFareCalculator
import Test.Tasty

main :: IO ()
main = do
  LocUpd.wrapTests (specs >=> defaultMain)

--  wrapTests $ \appEnv -> defaultMain $ locationUpdatesTree appEnv

specs :: LocUpd.AppEnv -> IO TestTree
specs appEnv = do
  let rideAPI = testGroup "Ride API" [startRide, endRideTests, cancelRide]
  let allocations =
        testGroup
          "Allocations"
          [ allocationTimeFinished,
            checkNotificationStatuses,
            onePoolTwoRide,
            twoAllocations,
            cancellation,
            reassignment
          ]
  return $
    testGroup
      "Unit tests"
      [ fareCalculator,
        rentalFareCalculator,
        allocations,
        rideAPI,
        distanceCalculation,
        LocUpd.locationUpdatesTree appEnv
      ]
