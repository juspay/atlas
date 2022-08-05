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

Module      :  Storage.Tabular.RentalFarePolicy
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Storage.Tabular.RentalFarePolicy where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Amount
import Beckn.Types.Id
import qualified Domain.Types.RentalFarePolicy as Domain
import qualified Domain.Types.Vehicle as Vehicle
import Storage.Tabular.Organization (OrganizationTId)
import Storage.Tabular.Vehicle ()

mkPersist
  defaultSqlSettings
  [defaultQQ|
    RentalFarePolicyT sql=rental_fare_policy
      id Text
      organizationId OrganizationTId
      vehicleVariant Vehicle.Variant
      baseFare Amount
      baseDistance Double
      baseDurationHr Int
      extraKmFare Amount
      extraMinuteFare Amount
      driverAllowanceForDay Amount Maybe
      deleted Bool
      Primary id
      deriving Generic
    |]

instance TEntityKey RentalFarePolicyT where
  type DomainKey RentalFarePolicyT = Id Domain.RentalFarePolicy
  fromKey (RentalFarePolicyTKey _id) = Id _id
  toKey (Id id) = RentalFarePolicyTKey id

instance TEntity RentalFarePolicyT Domain.RentalFarePolicy where
  fromTEntity entity = do
    let RentalFarePolicyT {..} = entityVal entity
    return $
      Domain.RentalFarePolicy
        { id = Id id,
          organizationId = fromKey organizationId,
          ..
        }
  toTType Domain.RentalFarePolicy {..} =
    RentalFarePolicyT
      { id = getId id,
        organizationId = toKey organizationId,
        deleted = False,
        ..
      }
  toTEntity a =
    Entity (toKey a.id) $ toTType a
