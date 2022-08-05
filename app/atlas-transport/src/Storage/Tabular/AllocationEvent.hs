{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}


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

Module      :  Storage.Tabular.AllocationEvent
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Storage.Tabular.AllocationEvent where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import qualified Domain.Types.AllocationEvent as Domain
import Storage.Tabular.Person (PersonTId)
import Storage.Tabular.RideBooking (RideBookingTId)

derivePersistField "Domain.AllocationEventType"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    AllocationEventT sql=allocation_event
      id Text
      driverId PersonTId Maybe
      eventType Domain.AllocationEventType
      timestamp UTCTime
      rideBookingId RideBookingTId
      Primary id
      deriving Generic
    |]

instance TEntityKey AllocationEventT where
  type DomainKey AllocationEventT = Id Domain.AllocationEvent
  fromKey (AllocationEventTKey _id) = Id _id
  toKey (Id id) = AllocationEventTKey id

instance TEntity AllocationEventT Domain.AllocationEvent where
  fromTEntity entity = do
    let AllocationEventT {..} = entityVal entity
    return $
      Domain.AllocationEvent
        { id = Id id,
          driverId = cast . fromKey <$> driverId,
          rideBookingId = fromKey rideBookingId,
          ..
        }
  toTType Domain.AllocationEvent {..} =
    AllocationEventT
      { id = getId id,
        driverId = toKey . cast <$> driverId,
        rideBookingId = toKey rideBookingId,
        ..
      }
  toTEntity a =
    Entity (toKey a.id) $ toTType a
