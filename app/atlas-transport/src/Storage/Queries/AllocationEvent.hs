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

Module      :  Storage.Queries.AllocationEvent
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Storage.Queries.AllocationEvent where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.AllocationEvent
import Domain.Types.RideBooking
import Storage.Tabular.AllocationEvent ()
import Types.App (Driver)
import Utils.Common

logAllocationEvent :: AllocationEventType -> Id RideBooking -> Maybe (Id Driver) -> SqlDB ()
logAllocationEvent eventType rideBookingId driverId = do
  uuid <- generateGUID
  now <- getCurrentTime
  Esq.create' $
    AllocationEvent
      { id = uuid,
        eventType = eventType,
        timestamp = now,
        driverId = driverId,
        rideBookingId = rideBookingId
      }
