{-# LANGUAGE OverloadedLabels #-}


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

Module      :  Mobility.Serviceability
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Mobility.Serviceability where

import Beckn.Types.MapSearch (LatLong (..))
import Common
import EulerHS.Prelude
import Mobility.Fixtures
import qualified Network.HTTP.Client as Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.Client
import Test.Hspec
import qualified "app-backend" Types.API.Search as AppBESearch
import Types.API.Serviceability
import Utils

ernakulamLocation :: LatLong
ernakulamLocation = LatLong 10.0739 76.2733

keralaLocation :: LatLong
keralaLocation = LatLong 10.5449 76.4356

karnatakaLocation :: LatLong
karnatakaLocation = LatLong 12.4725 75.8328

verifyServiceability :: (Eq a, Show a) => a -> Either ClientError a -> IO ()
verifyServiceability expectedValue = \case
  Left _ -> expectationFailure "Expected success response"
  Right value -> value `shouldBe` expectedValue

serviceableOrigin :: ClientEnv -> IO ()
serviceableOrigin appClientEnv =
  runClient appClientEnv (originServiceability appRegistrationToken req)
    >>= verifyServiceability (ServiceabilityRes True)
  where
    req = ServiceabilityReq ernakulamLocation

nonServiceableOrigin :: ClientEnv -> IO ()
nonServiceableOrigin appClientEnv =
  runClient appClientEnv (originServiceability appRegistrationToken req)
    >>= verifyServiceability (ServiceabilityRes False)
  where
    req = ServiceabilityReq keralaLocation

serviceableDestination :: ClientEnv -> IO ()
serviceableDestination appClientEnv =
  runClient appClientEnv (destinationServiceability appRegistrationToken req)
    >>= verifyServiceability (ServiceabilityRes True)
  where
    req = ServiceabilityReq keralaLocation

nonServiceableDestination :: ClientEnv -> IO ()
nonServiceableDestination appClientEnv =
  runClient appClientEnv (destinationServiceability appRegistrationToken req)
    >>= verifyServiceability (ServiceabilityRes False)
  where
    req = ServiceabilityReq karnatakaLocation

nonServiceableSearchRequest :: ClientEnv -> IO ()
nonServiceableSearchRequest appClientEnv = do
  let updatedSearchReq = case searchReq of
        AppBESearch.OneWaySearch req ->
          AppBESearch.OneWaySearch $
            req
              & #origin . #gps .~ keralaLocation
              & #destination . #gps .~ karnatakaLocation
        AppBESearch.RentalSearch req ->
          AppBESearch.RentalSearch $
            req
              & #origin . #gps .~ keralaLocation
  result <- runClient appClientEnv (searchServices appRegistrationToken updatedSearchReq)
  verifyError 400 "RIDE_NOT_SERVICEABLE" result

spec :: Spec
spec = do
  appManager <- runIO $ Client.newManager tlsManagerSettings
  let appBaseUrl = getAppBaseUrl
      appClientEnv = mkClientEnv appManager appBaseUrl

  describe "Testing Serviceability API" do
    it "Serviceable origin" $ serviceableOrigin appClientEnv
    it "Non-serviceable origin" $ nonServiceableOrigin appClientEnv
    it "Serviceable destination" $ serviceableDestination appClientEnv
    it "Non-serviceable destination" $ nonServiceableDestination appClientEnv
    it "Non-serviceable search request" $ nonServiceableSearchRequest appClientEnv
