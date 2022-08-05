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

Module      :  Storage.Tabular.CallStatus
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Storage.Tabular.CallStatus where

import Beckn.External.Exotel.Types (ExotelCallStatus)
import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import qualified Domain.Types.CallStatus as Domain
import Storage.Tabular.Ride (RideTId)

mkPersist
  defaultSqlSettings
  [defaultQQ|
    CallStatusT sql=call_status
      id Text
      exotelCallSid Text
      rideId RideTId
      status ExotelCallStatus
      recordingUrl Text Maybe
      conversationDuration Int
      createdAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey CallStatusT where
  type DomainKey CallStatusT = Id Domain.CallStatus
  fromKey (CallStatusTKey _id) = Id _id
  toKey (Id id) = CallStatusTKey id

instance TEntity CallStatusT Domain.CallStatus where
  fromTEntity entity = do
    let CallStatusT {..} = entityVal entity
    return $
      Domain.CallStatus
        { id = Id id,
          rideId = fromKey rideId,
          ..
        }
  toTType Domain.CallStatus {..} =
    CallStatusT
      { id = getId id,
        rideId = toKey rideId,
        ..
      }
  toTEntity a =
    Entity (toKey a.id) $ toTType a
