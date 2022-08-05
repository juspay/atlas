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

Module      :  Storage.Tabular.DriverInformation
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Storage.Tabular.DriverInformation where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import qualified Domain.Types.DriverInformation as Domain
import Domain.Types.Person (Person)
import Storage.Tabular.Person (PersonTId)

mkPersist
  defaultSqlSettings
  [defaultQQ|
    DriverInformationT sql=driver_information
      driverId PersonTId
      active Bool
      onRide Bool
      enabled Bool
      optForRental Bool
      createdAt UTCTime
      updatedAt UTCTime
      canDowngradeToSedan Bool
      canDowngradeToHatchback Bool
      Primary driverId
      deriving Generic
    |]

instance TEntityKey DriverInformationT where
  type DomainKey DriverInformationT = Id Person
  fromKey (DriverInformationTKey _id) = fromKey _id
  toKey id = DriverInformationTKey $ toKey id

instance TEntity DriverInformationT Domain.DriverInformation where
  fromTEntity entity = do
    let DriverInformationT {..} = entityVal entity
    return $
      Domain.DriverInformation
        { driverId = fromKey driverId,
          ..
        }
  toTType Domain.DriverInformation {..} =
    DriverInformationT
      { driverId = toKey driverId,
        ..
      }
  toTEntity a =
    Entity (toKey a.driverId) $ toTType a
