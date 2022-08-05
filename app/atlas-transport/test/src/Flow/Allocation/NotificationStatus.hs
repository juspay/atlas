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

Module      :  Flow.Allocation.NotificationStatus
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Flow.Allocation.NotificationStatus where

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

driverPool1 :: [Id Driver]
driverPool1 = [Id "driver01", Id "driver02"]

driverPoolPerRide :: Map (Id SRB.RideBooking) [Id Driver]
driverPoolPerRide = Map.fromList [(rideBooking01Id, driverPool1)]

checkNotificationStatuses :: TestTree
checkNotificationStatuses = testCaseSteps "Check NotificationStatus" $ \step -> do
  r@Repository {..} <- initRepository
  addRideBooking r rideBooking01Id 0
  addDriverPool r driverPoolPerRide
  addRequest Allocation r rideBooking01Id
  void $ process (handle r) org1 numRequestsToProcess
  step "All Notified"
  checkNotificationStatus r rideBooking01Id (Id "driver01") Notified
  checkNotificationStatus r rideBooking01Id (Id "driver02") Notified
  step "Driver01 Rejected"
  addResponse r rideBooking01Id (Id "driver01") RideBooking.REJECT
  void $ process (handle r) org1 numRequestsToProcess
  checkNotificationStatus r rideBooking01Id (Id "driver01") Rejected
  checkNotificationStatus r rideBooking01Id (Id "driver02") Notified
  step "Driver01 Ignored"
  threadDelay 1200000
  addDriverInPool r rideBooking01Id (Id "driver03")
  void $ process (handle r) org1 numRequestsToProcess
  checkNotificationStatus r rideBooking01Id (Id "driver02") Ignored
  step "Driver03 No NotificationStatus"
  addResponse r rideBooking01Id (Id "driver03") RideBooking.ACCEPT
  void $ process (handle r) org1 numRequestsToProcess
  assignments2 <- readIORef assignmentsVar
  assignments2 @?= [(rideBooking01Id, Id "driver03")]
  checkRideStatus r rideBooking01Id Assigned
  checkFreeNotificationStatus r rideBooking01Id (Id "driver03")
