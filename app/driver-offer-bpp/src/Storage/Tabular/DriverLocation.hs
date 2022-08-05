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

Module      :  Storage.Tabular.DriverLocation
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Storage.Tabular.DriverLocation where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import qualified Domain.Types.DriverLocation as Domain
import Domain.Types.Person (Person)
import Storage.Tabular.Person (PersonTId)

mkPersist
  defaultSqlSettings
  [defaultQQ|
    DriverLocationT sql=driver_location
      driverId PersonTId
      lat Double
      lon Double
      point Point
      createdAt UTCTime
      updatedAt UTCTime
      Primary driverId
      UniqueDriverLocationId driverId
      deriving Generic
    |]

instance TEntityKey DriverLocationT where
  type DomainKey DriverLocationT = Id Person
  fromKey (DriverLocationTKey _id) = fromKey _id
  toKey id = DriverLocationTKey $ toKey id

instance TEntity DriverLocationT Domain.DriverLocation where
  fromTEntity entity = do
    let DriverLocationT {..} = entityVal entity
    return $
      Domain.DriverLocation
        { driverId = fromKey driverId,
          ..
        }
  toTType Domain.DriverLocation {..} =
    DriverLocationT
      { driverId = toKey driverId,
        point = Point,
        ..
      }
  toTEntity a =
    Entity (toKey a.driverId) $ toTType a
