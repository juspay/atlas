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

Module      :  Flow.Allocation.Reassignment
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Flow.Allocation.Reassignment where

import Beckn.Types.Id
import qualified Data.Map as Map
import qualified Domain.Types.RideBooking as SRB
import EulerHS.Prelude hiding (id)
import Flow.Allocation.Internal
import Services.Allocation.Allocation
import Test.Tasty
import Test.Tasty.HUnit
import qualified Types.API.RideBooking as RideBooking
import Types.App

rideBooking01Id :: Id SRB.RideBooking
rideBooking01Id = Id "rideBooking01"

rideBooking02Id :: Id SRB.RideBooking
rideBooking02Id = Id "rideBooking02"

driverPool1 :: [Id Driver]
driverPool1 = [Id "driver01", Id "driver02"]

driverPool2 :: [Id Driver]
driverPool2 = [Id "driver01", Id "driver03"]

driverPoolPerRide :: Map (Id SRB.RideBooking) [Id Driver]
driverPoolPerRide = Map.fromList [(rideBooking01Id, driverPool1), (rideBooking02Id, driverPool2)]

reassignmentRide :: TestTree
reassignmentRide = testCase "Reassignment rideBooking after cancellation" $ do
  r@Repository {..} <- initRepository
  addRideBooking r rideBooking01Id 0
  addDriverPool r driverPoolPerRide
  addRequest Allocation r rideBooking01Id
  void $ process (handle r) org1 numRequestsToProcess
  addResponse r rideBooking01Id (Id "driver01") RideBooking.ACCEPT
  void $ process (handle r) org1 numRequestsToProcess
  addRequest Cancellation r rideBooking01Id
  void $ process (handle r) org1 numRequestsToProcess
  updateRideBooking r rideBooking01Id AwaitingReassignment
  addRequest Allocation r rideBooking01Id
  void $ process (handle r) org1 numRequestsToProcess
  checkNotificationStatus r rideBooking01Id (Id "driver02") Notified
  addResponse r rideBooking01Id (Id "driver02") RideBooking.ACCEPT
  void $ process (handle r) org1 numRequestsToProcess
  assignments <- readIORef assignmentsVar
  assignments @?= [(rideBooking01Id, Id "driver02"), (rideBooking01Id, Id "driver01")]
  checkRideStatus r rideBooking01Id Assigned
  onRide <- readIORef onRideVar
  onRide @?= [Id "driver02"]

reassignmentDriver :: TestTree
reassignmentDriver = testCase "Reassignment driver after cancellation" $ do
  r@Repository {..} <- initRepository
  addRideBooking r rideBooking01Id 0
  addRideBooking r rideBooking02Id 0
  addDriverPool r driverPoolPerRide
  addRequest Allocation r rideBooking01Id
  void $ process (handle r) org1 numRequestsToProcess
  addResponse r rideBooking01Id (Id "driver01") RideBooking.ACCEPT
  void $ process (handle r) org1 numRequestsToProcess
  addRequest Cancellation r rideBooking01Id
  void $ process (handle r) org1 numRequestsToProcess
  addRequest Allocation r rideBooking02Id
  void $ process (handle r) org1 numRequestsToProcess
  addResponse r rideBooking02Id (Id "driver01") RideBooking.ACCEPT
  void $ process (handle r) org1 numRequestsToProcess
  assignments <- readIORef assignmentsVar
  assignments @?= [(rideBooking02Id, Id "driver01"), (rideBooking01Id, Id "driver01")]
  checkRideStatus r rideBooking02Id Assigned
  onRide <- readIORef onRideVar
  onRide @?= [Id "driver01"]

reassignment :: TestTree
reassignment =
  testGroup
    "Reassignment after cancellation"
    [ reassignmentRide,
      reassignmentDriver
    ]
