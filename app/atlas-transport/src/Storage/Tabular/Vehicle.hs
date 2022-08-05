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

Module      :  Storage.Tabular.Vehicle
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Storage.Tabular.Vehicle where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import qualified Domain.Types.Vehicle as Domain
import qualified Storage.Tabular.Organization as TOrg

derivePersistField "Domain.Category"
derivePersistField "Domain.Variant"
derivePersistField "Domain.EnergyType"
derivePersistField "Domain.RegistrationCategory"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    VehicleT sql=vehicle
      id Text
      organizationId TOrg.OrganizationTId
      variant Domain.Variant
      model Text
      color Text
      registrationNo Text
      capacity Int Maybe
      category Domain.Category Maybe
      make Text Maybe
      size Text Maybe
      energyType Domain.EnergyType Maybe
      registrationCategory Domain.RegistrationCategory Maybe
      createdAt UTCTime
      updatedAt UTCTime
      Primary id
      Unique VehicleRegistrationNo
      deriving Generic
    |]

instance TEntityKey VehicleT where
  type DomainKey VehicleT = Id Domain.Vehicle
  fromKey (VehicleTKey _id) = Id _id
  toKey (Id id) = VehicleTKey id

instance TEntity VehicleT Domain.Vehicle where
  fromTEntity entity = do
    let VehicleT {..} = entityVal entity
    return $
      Domain.Vehicle
        { id = Id id,
          organizationId = fromKey organizationId,
          ..
        }
  toTType Domain.Vehicle {..} =
    VehicleT
      { id = getId id,
        organizationId = toKey organizationId,
        ..
      }
  toTEntity a =
    Entity (toKey a.id) $ toTType a
