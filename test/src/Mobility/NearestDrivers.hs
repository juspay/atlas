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

Module      :  Mobility.NearestDrivers
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Mobility.NearestDrivers (spec) where

import qualified "atlas-transport" App.Types as BecknTransport
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Flow (FlowR)
import Beckn.Types.Id
import Beckn.Types.MapSearch (LatLong (..))
import qualified "atlas-transport" Domain.Types.FareProduct as SFP
import Domain.Types.Vehicle
import EulerHS.Prelude
import qualified "atlas-transport" Storage.Queries.DriverInformation as Q
import qualified "atlas-transport" Storage.Queries.Person as Q
import Test.Hspec
import Utils

spec :: Spec
spec = do
  describe "getNearestDrivers function"
    . beforeAll_ (runTransporterFlow "Turn on drivers" $ setDriversActive True)
    . afterAll_ (runTransporterFlow "Turn off drivers." $ setDriversActive False)
    $ do
      it "Test ordering" testOrder
      it "Test radius filtration" testInRadius
      it "Test outside radius filtration" testNotInRadius
      it "Test downgrading driver with SUV ride request" testDowngradingDriverWithSUV
      it "Test downgrading driver with sedan ride request" testDowngradingDriverWithSedan
      it "Test downgrading driver with hatchback ride request" testDowngradingDriverWithHatchback
      it "Test isRental" testIsRental
      it "Test notRental" testNotRental

testOrder :: IO ()
testOrder = do
  res <-
    runTransporterFlow "Test ordering" $
      Q.getNearestDrivers pickupPoint 5000 org1 (Just SUV) SFP.ONE_WAY <&> getIds
  res `shouldBe` [closestDriver, furthestDriver]

testInRadius :: IO ()
testInRadius = do
  res <-
    runTransporterFlow "Test radius filtration" $
      Q.getNearestDrivers pickupPoint 800 org1 (Just SUV) SFP.ONE_WAY <&> getIds
  res `shouldBe` [closestDriver]

testNotInRadius :: IO ()
testNotInRadius = do
  res <-
    runTransporterFlow "Test outside radius filtration" $
      Q.getNearestDrivers pickupPoint 0 org1 (Just SUV) SFP.ONE_WAY <&> getIds
  res `shouldBe` []

testDowngradingDriverWithSUV :: IO ()
testDowngradingDriverWithSUV = do
  res <-
    runTransporterFlow "Test downgrading driver with SUV ride request" $
      Q.getNearestDrivers pickupPoint 10000 org1 (Just SUV) SFP.ONE_WAY <&> getIds
  res `shouldBe` [closestDriver, furthestDriver, suvDriver]

testDowngradingDriverWithSedan :: IO ()
testDowngradingDriverWithSedan = do
  res <-
    runTransporterFlow "Test downgrading driver with sedan ride request" $
      Q.getNearestDrivers pickupPoint 10000 org1 (Just SEDAN) SFP.ONE_WAY <&> getIds
  res `shouldBe` [suvDriver, sedanDriver]

testDowngradingDriverWithHatchback :: IO ()
testDowngradingDriverWithHatchback = do
  res <-
    runTransporterFlow "Test downgrading driver with hatchback ride request" $
      Q.getNearestDrivers pickupPoint 10000 org1 (Just HATCHBACK) SFP.ONE_WAY <&> getIds
  res `shouldBe` [suvDriver, sedanDriver, hatchbackDriver]

testIsRental :: IO ()
testIsRental = do
  runTransporterFlow "suvDriver is Rental" $ setSuvDriverRental True
  res <-
    runTransporterFlow "Test isRental" $
      Q.getNearestDrivers pickupPoint 10000 org1 (Just HATCHBACK) SFP.RENTAL <&> getIds
  res `shouldBe` [suvDriver]

testNotRental :: IO ()
testNotRental = do
  runTransporterFlow "suvDriver not Rental" $ setSuvDriverRental False
  res <-
    runTransporterFlow "Test notRental" $
      Q.getNearestDrivers pickupPoint 10000 org1 (Just HATCHBACK) SFP.RENTAL <&> getIds
  res `shouldBe` []

getIds :: [Q.DriverPoolResult] -> [Text]
getIds = map (getId . (.driverId))

pickupPoint :: LatLong
pickupPoint = LatLong 12.994927 77.596386

org1 :: forall k. Id k
org1 = Id "7f7896dd-787e-4a0b-8675-e9e6fe93bb8f"

furthestDriver :: Text
furthestDriver = "001093df-4f7c-440f-b-furthest_driver"

closestDriver :: Text
closestDriver = "002093df-4f7c-440f-ba-closest_driver"

otherDriver :: Text
otherDriver = "003093df-4f7c-440f-bada-other_driver"

-- distance to next drivers is more than 5000
suvDriver :: Text
suvDriver = "003093df-4f7c-440f-bada-4-suv_driver"

sedanDriver :: Text
sedanDriver = "003093df-4f7c-440f-bada-sedan_driver"

hatchbackDriver :: Text
hatchbackDriver = "003093df-4f7c-440f--hatchback_driver"

setDriversActive :: Bool -> FlowR BecknTransport.AppEnv ()
setDriversActive isActive = Esq.runTransaction $ do
  let drivers = [furthestDriver, closestDriver, otherDriver, suvDriver, sedanDriver, hatchbackDriver]
  forM_ drivers (\driver -> Q.updateActivity (Id driver) isActive)

setSuvDriverRental :: Bool -> FlowR BecknTransport.AppEnv ()
setSuvDriverRental isRental = Esq.runTransaction $ do
  _ <- Q.updateRental (Id suvDriver) isRental
  return ()
