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

Module      :  Storage.Tabular.ParkingLocation
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Storage.Tabular.ParkingLocation where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import qualified Domain.ParkingLocation as Domain

mkPersist
  defaultSqlSettings
  [defaultQQ|
    ParkingLocationT sql=parking_location
      id Text
      idFromBpp Text
      lat Double
      lon Double
      name Text
      streetAddress Text
      locality Text
      city Text Maybe
      state Text
      country Text
      areaCode Text
      createdAt UTCTime
      deriving Generic
      Primary id
    |]

instance TEntityKey ParkingLocationT where
  type DomainKey ParkingLocationT = Id Domain.ParkingLocation
  fromKey (ParkingLocationTKey _id) = Id _id
  toKey id = ParkingLocationTKey id.getId

instance TEntity ParkingLocationT Domain.ParkingLocation where
  fromTEntity entity = do
    let ParkingLocationT {..} = entityVal entity
    return $
      Domain.ParkingLocation
        { id = Id id,
          ..
        }
  toTType Domain.ParkingLocation {..} =
    ParkingLocationT
      { id = id.getId,
        ..
      }
  toTEntity a =
    Entity (toKey a.id) $ toTType a
