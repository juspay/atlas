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

Module      :  Flow.RideAPI.CancelRide
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Flow.RideAPI.CancelRide where

import qualified Beckn.Types.APISuccess as APISuccess
import Beckn.Types.Id
import Domain.Types.CancellationReason
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Ride as Ride
import EulerHS.Prelude
import qualified Fixtures
import qualified Product.RideAPI.Handlers.CancelRide as CancelRide
import Test.Hspec
import Test.Tasty
import Test.Tasty.HUnit
import qualified Types.API.Ride as RideAPI
import Types.Error
import Utils.GuidGenerator ()
import Utils.SilentLogger ()

handle :: CancelRide.ServiceHandle IO
handle =
  CancelRide.ServiceHandle
    { findRideById = \_rideId -> pure $ Just ride,
      findById = \_personid -> pure $ Just Fixtures.defaultDriver,
      cancelRide = \_rideReq _requestedByAdmin -> pure ()
    }

ride :: Ride.Ride
ride =
  Fixtures.defaultRide
    { Ride.status = Ride.NEW
    }

cancelRide :: TestTree
cancelRide =
  testGroup
    "Ride cancellation"
    [ successfulCancellationByDriver,
      successfulCancellationByAdmin,
      failedCancellationByAnotherDriver,
      failedCancellationWhenQuoteStatusIsWrong
    ]

runHandler :: CancelRide.ServiceHandle IO -> Id Person.Person -> Id Ride.Ride -> RideAPI.CancelRideReq -> IO APISuccess.APISuccess
runHandler = CancelRide.cancelRideHandler

someCancelRideReq :: RideAPI.CancelRideReq
someCancelRideReq =
  RideAPI.CancelRideReq (CancellationReasonCode "OTHER") $ Just "Your car is not flying."

successfulCancellationByDriver :: TestTree
successfulCancellationByDriver =
  testCase "Cancel successfully if requested by driver executor" $ do
    runHandler handle (Id "1") "1" someCancelRideReq
      `shouldReturn` APISuccess.Success

successfulCancellationByAdmin :: TestTree
successfulCancellationByAdmin =
  testCase "Cancel successfully if requested by admin" $ do
    runHandler handleCase (Id "1") "1" someCancelRideReq
      `shouldReturn` APISuccess.Success
  where
    handleCase = handle {CancelRide.findById = \_personId -> pure $ Just admin}
    admin =
      Fixtures.defaultDriver{id = Id "adminId",
                             role = Person.ADMIN
                            }

failedCancellationByAnotherDriver :: TestTree
failedCancellationByAnotherDriver =
  testCase "Fail cancellation if requested by driver not executor" $ do
    runHandler handleCase (Id "driverNotExecutorId") "1" someCancelRideReq
      `shouldThrow` (== NotAnExecutor)
  where
    handleCase = handle {CancelRide.findById = \_personId -> pure $ Just driverNotExecutor}
    driverNotExecutor = Fixtures.defaultDriver{id = Id "driverNotExecutorId"}

failedCancellationWhenQuoteStatusIsWrong :: TestTree
failedCancellationWhenQuoteStatusIsWrong =
  testCase "Fail cancellation if ride has inappropriate ride status" $ do
    runHandler handleCase (Id "1") "1" someCancelRideReq
      `shouldThrow` (\(RideInvalidStatus _) -> True)
  where
    handleCase = handle {CancelRide.findRideById = \_rideId -> pure $ Just completedPI}
    completedPI = ride{status = Ride.COMPLETED}
