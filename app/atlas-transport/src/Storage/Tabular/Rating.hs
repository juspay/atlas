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

Module      :  Storage.Tabular.Rating
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Storage.Tabular.Rating where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import qualified Domain.Types.Rating as Domain
import Storage.Tabular.Person (PersonTId)
import Storage.Tabular.Ride (RideTId)

mkPersist
  defaultSqlSettings
  [defaultQQ|
    RatingT sql=rating
      id Text
      rideId RideTId
      driverId PersonTId
      ratingValue Int
      createdAt UTCTime
      updatedAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey RatingT where
  type DomainKey RatingT = Id Domain.Rating
  fromKey (RatingTKey _id) = Id _id
  toKey (Id id) = RatingTKey id

instance TEntity RatingT Domain.Rating where
  fromTEntity entity = do
    let RatingT {..} = entityVal entity
    return $
      Domain.Rating
        { id = Id id,
          driverId = fromKey driverId,
          rideId = fromKey rideId,
          ..
        }
  toTType Domain.Rating {..} =
    RatingT
      { id = getId id,
        driverId = toKey driverId,
        rideId = toKey rideId,
        ..
      }
  toTEntity a =
    Entity (toKey a.id) $ toTType a
