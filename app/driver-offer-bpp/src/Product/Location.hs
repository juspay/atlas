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

Module      :  Product.Location
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Product.Location where

import Beckn.Prelude
import Beckn.Storage.Esqueleto (Transactionable (runTransaction))
import Beckn.Types.APISuccess (APISuccess (..))
import Beckn.Types.Common
import Beckn.Types.Error
import Beckn.Types.Id
import Beckn.Utils.Logging
import qualified Data.List.NonEmpty as NE
import qualified Domain.Types.Person as Person
import Environment
import GHC.Records.Extra
import Storage.Queries.DriverLocation (upsertGpsCoord)
import qualified Storage.Queries.Person as Person
import Types.API.Location as Location
import Utils.Common hiding (id)

updateLocation :: Id Person.Person -> UpdateLocationReq -> FlowHandler APISuccess
updateLocation driverId waypoints = withFlowHandlerAPI $
  withLogTag "driverLocationUpdate" $ do
    logInfo $ "got location updates: " <> getId driverId <> " " <> encodeToText waypoints
    driver <-
      Person.findById driverId
        >>= fromMaybeM (PersonNotFound driverId.getId)
    unless (driver.role == Person.DRIVER) $ throwError AccessDenied
    let currPoint = NE.last waypoints
    runTransaction $ upsertGpsCoord driver.id currPoint.pt currPoint.ts
    pure Success
