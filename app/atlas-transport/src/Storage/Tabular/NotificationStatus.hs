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

Module      :  Storage.Tabular.NotificationStatus
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Storage.Tabular.NotificationStatus where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import qualified Domain.Types.NotificationStatus as Domain
import Storage.Tabular.Person (PersonTId)
import Storage.Tabular.RideBooking (RideBookingTId)

derivePersistField "Domain.AnswerStatus"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    NotificationStatusT sql=notification_status
      id Text
      rideBookingId RideBookingTId
      driverId PersonTId
      status Domain.AnswerStatus
      expiresAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey NotificationStatusT where
  type DomainKey NotificationStatusT = Id Domain.NotificationStatus
  fromKey (NotificationStatusTKey _id) = Id _id
  toKey (Id id) = NotificationStatusTKey id

instance TEntity NotificationStatusT Domain.NotificationStatus where
  fromTEntity entity = do
    let NotificationStatusT {..} = entityVal entity
    return $
      Domain.NotificationStatus
        { id = Id id,
          driverId = cast $ fromKey driverId,
          rideBookingId = fromKey rideBookingId,
          ..
        }
  toTType Domain.NotificationStatus {..} =
    NotificationStatusT
      { id = getId id,
        driverId = toKey $ cast driverId,
        rideBookingId = toKey rideBookingId,
        ..
      }
  toTEntity a =
    Entity (toKey a.id) $ toTType a
