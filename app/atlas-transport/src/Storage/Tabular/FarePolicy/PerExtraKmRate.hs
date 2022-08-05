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

Module      :  Storage.Tabular.FarePolicy.PerExtraKmRate
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Storage.Tabular.FarePolicy.PerExtraKmRate where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import qualified Domain.Types.FarePolicy.PerExtraKmRate as Domain
import Domain.Types.Organization (Organization)
import qualified Domain.Types.Vehicle as Vehicle
import Storage.Tabular.Organization (OrganizationTId)
import Storage.Tabular.Vehicle ()

mkPersist
  defaultSqlSettings
  [defaultQQ|
    PerExtraKmRateT sql=fare_policy_per_extra_km_rate
      id Text
      vehicleVariant Vehicle.Variant
      organizationId OrganizationTId
      distanceRangeStart Double
      fare Double
      Primary id
      deriving Generic
    |]

instance TEntityKey PerExtraKmRateT where
  type DomainKey PerExtraKmRateT = Id Domain.PerExtraKmRate
  fromKey (PerExtraKmRateTKey _id) = Id _id
  toKey (Id id) = PerExtraKmRateTKey id

type FullPerExtraKmRate = (Id Domain.PerExtraKmRate, Id Organization, Vehicle.Variant, Domain.PerExtraKmRate)

instance TEntity PerExtraKmRateT FullPerExtraKmRate where
  fromTEntity entity = do
    let PerExtraKmRateT {..} = entityVal entity
    return
      ( Id id,
        fromKey organizationId,
        vehicleVariant,
        Domain.PerExtraKmRate
          { distanceRangeStart = toRational distanceRangeStart,
            fare = toRational fare,
            ..
          }
      )
  toTType (id, orgId, vehVar, Domain.PerExtraKmRate {..}) =
    PerExtraKmRateT
      { id = getId id,
        organizationId = toKey orgId,
        vehicleVariant = vehVar,
        distanceRangeStart = fromRational distanceRangeStart,
        fare = fromRational fare,
        ..
      }
  toTEntity a@(id, _, _, _) =
    Entity (toKey id) $ toTType a
