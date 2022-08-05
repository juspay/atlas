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

Module      :  Storage.Tabular.BusinessEvent
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Storage.Tabular.BusinessEvent where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import qualified Domain.Types.BusinessEvent as Domain
import Domain.Types.Vehicle (Variant)
import Storage.Tabular.Person (PersonTId)
import Storage.Tabular.Ride (RideTId)
import Storage.Tabular.RideBooking (RideBookingTId)

derivePersistField "Domain.EventType"
derivePersistField "Domain.WhenPoolWasComputed"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    BusinessEventT sql=business_event
      id Text
      driverId PersonTId Maybe
      eventType Domain.EventType
      timeStamp UTCTime
      rideBookingId RideBookingTId Maybe
      whenPoolWasComputed Domain.WhenPoolWasComputed Maybe
      vehicleVariant Variant Maybe
      distance Double Maybe
      duration Double Maybe
      rideId RideTId Maybe
      Primary id
      deriving Generic
    |]

instance TEntityKey BusinessEventT where
  type DomainKey BusinessEventT = Id Domain.BusinessEvent
  fromKey (BusinessEventTKey _id) = Id _id
  toKey (Id id) = BusinessEventTKey id

instance TEntity BusinessEventT Domain.BusinessEvent where
  fromTEntity entity = do
    let BusinessEventT {..} = entityVal entity
    return $
      Domain.BusinessEvent
        { id = Id id,
          driverId = cast . fromKey <$> driverId,
          rideBookingId = fromKey <$> rideBookingId,
          rideId = fromKey <$> rideId,
          ..
        }
  toTType Domain.BusinessEvent {..} =
    BusinessEventT
      { id = getId id,
        driverId = toKey . cast <$> driverId,
        rideBookingId = toKey <$> rideBookingId,
        rideId = toKey <$> rideId,
        ..
      }
  toTEntity a =
    Entity (toKey a.id) $ toTType a
