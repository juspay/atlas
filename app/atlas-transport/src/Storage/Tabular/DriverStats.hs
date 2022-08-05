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

Module      :  Storage.Tabular.DriverStats
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Storage.Tabular.DriverStats where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import qualified Domain.Types.DriverStats as Domain
import Storage.Tabular.Person (PersonTId)
import Types.App (Driver)

mkPersist
  defaultSqlSettings
  [defaultQQ|
    DriverStatsT sql=driver_stats
      driverId PersonTId
      idleSince UTCTime
      Primary driverId
      deriving Generic
    |]

instance TEntityKey DriverStatsT where
  type DomainKey DriverStatsT = Id Driver
  fromKey (DriverStatsTKey _id) = cast $ fromKey _id
  toKey id = DriverStatsTKey . toKey $ cast id

instance TEntity DriverStatsT Domain.DriverStats where
  fromTEntity entity = do
    let DriverStatsT {..} = entityVal entity
    return $
      Domain.DriverStats
        { driverId = cast $ fromKey driverId,
          ..
        }
  toTType Domain.DriverStats {..} =
    DriverStatsT
      { driverId = toKey . cast $ driverId,
        ..
      }
  toTEntity a =
    Entity (toKey a.driverId) $ toTType a
