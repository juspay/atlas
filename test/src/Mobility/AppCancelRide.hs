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

Module      :  Mobility.AppCancelRide
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Mobility.AppCancelRide where

import Common (getAppBaseUrl)
import qualified "app-backend" Domain.Types.CancellationReason as AppCR
import qualified "app-backend" Domain.Types.RideBooking as AppRB
import EulerHS.Prelude
import HSpec
import Mobility.Fixtures
import Mobility.SuccessFlow (doAnAppSearch)
import Utils

spec :: Spec
spec = do
  clients <- runIO $ mkMobilityClients getAppBaseUrl getTransporterBaseUrl
  describe "Testing App and Transporter APIs" $ do
    it "Testing API flow for ride cancelled by App" . withBecknClients clients $ do
      (_, bRideBookingId) <- doAnAppSearch
      void . callBPP $ setDriverOnline driverToken1 True

      -- cancel request initiated by App
      void . callBAP $ cancelRide bRideBookingId appRegistrationToken (buildAppCancelReq AppCR.OnConfirm)

      void . poll $
        callBAP (appRideBookingStatus bRideBookingId appRegistrationToken)
          <&> (.status)
          >>= (`shouldBe` AppRB.CANCELLED)
          <&> Just

      void . callBPP $ setDriverOnline driverToken1 False
