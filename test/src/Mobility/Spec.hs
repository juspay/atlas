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

Module      :  Mobility.Spec
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Mobility.Spec where

import EulerHS.Prelude
import qualified Mobility.AppCancelRide as CR
import qualified Mobility.DriverCancelRide as DCR
import qualified Mobility.DriversRejectRide as DRR
import qualified Mobility.HealthCheck as HC
import qualified Mobility.LocationUpdates as LU
import qualified Mobility.NearestDrivers as ND
import qualified Mobility.Serviceability as SRV
import qualified Mobility.SuccessFlow as SF
import Test.Tasty
import Test.Tasty.Hspec hiding (after)

mkTestTree :: IO TestTree
mkTestTree = do
  hcSpec <- testSpec "HealthCheck" HC.spec
  sfSpec <- testSpec "SuccessFlow" SF.spec
  drrSpec <- testSpec "DriversRejectRide" DRR.spec
  crSpec <- testSpec "AppCancelRide" CR.spec
  dcrSpec <- testSpec "DriverCancelRide" DCR.spec
  srvSpec <- testSpec "Serviceability" SRV.spec
  ndSpec <- testSpec "NearestDriver" ND.spec

  -- these tests pass only when the real google maps api key is supplied
  locationUpdatesSpec <- testSpec "LocationUpdates" LU.spec
  ------------------------------------------------------------------
  return $
    testGroup
      "Mobility"
      [ hcSpec,
        after AllSucceed "HealthCheck" $
          testGroup
            "APIs"
            [ ndSpec,
              srvSpec,
              sfSpec,
              after AllSucceed "SuccessFlow" $
                testGroup
                  "Flows"
                  [ drrSpec,
                    crSpec,
                    dcrSpec
                  ]
            ],
        testGroup "LocationUpdates" [locationUpdatesSpec]
      ]
