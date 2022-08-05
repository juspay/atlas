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

Module      :  Storage.Tabular.RideRequest
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Storage.Tabular.RideRequest where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import qualified Domain.Types.RideRequest as Domain
import Storage.Tabular.RideBooking (RideBookingTId)

derivePersistField "Domain.RideRequestType"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    RideRequestT sql=ride_request
      id Text
      rideBookingId RideBookingTId
      shortOrgId Text
      createdAt UTCTime
      reqType Domain.RideRequestType sql=type
      info Text Maybe
      Primary id
      deriving Generic
    |]

instance TEntityKey RideRequestT where
  type DomainKey RideRequestT = Id Domain.RideRequest
  fromKey (RideRequestTKey _id) = Id _id
  toKey (Id id) = RideRequestTKey id

instance TEntity RideRequestT Domain.RideRequest where
  fromTEntity entity = do
    let RideRequestT {..} = entityVal entity
    return $
      Domain.RideRequest
        { id = Id id,
          rideBookingId = fromKey rideBookingId,
          shortOrgId = ShortId shortOrgId,
          _type = reqType,
          ..
        }
  toTType Domain.RideRequest {..} =
    RideRequestT
      { id = getId id,
        rideBookingId = toKey rideBookingId,
        shortOrgId = getShortId shortOrgId,
        reqType = _type,
        ..
      }
  toTEntity a =
    Entity (toKey a.id) $ toTType a
