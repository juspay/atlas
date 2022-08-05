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

Module      :  Storage.Tabular.FarePolicy
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Storage.Tabular.FarePolicy where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import qualified Domain.Types.FarePolicy as Domain
import qualified Storage.Queries.FarePolicy.PerExtraKmRate as QExtraKmRate
import Storage.Tabular.FarePolicy.PerExtraKmRate ()
import Storage.Tabular.Organization (OrganizationTId)
import Storage.Tabular.Vehicle ()

mkPersist
  defaultSqlSettings
  [defaultQQ|
    FarePolicyT sql=fare_policy
      id Text
      organizationId OrganizationTId
      baseFare Double Maybe
      nightShiftStart TimeOfDay Maybe
      nightShiftEnd TimeOfDay Maybe
      nightShiftRate Double Maybe
      createdAt UTCTime
      updatedAt UTCTime
      UniqueFarePolicyId id
      Primary id
      deriving Generic
    |]

instance TEntityKey FarePolicyT where
  type DomainKey FarePolicyT = Id Domain.FarePolicy
  fromKey (FarePolicyTKey _id) = Id _id
  toKey (Id id) = FarePolicyTKey id

instance TEntity FarePolicyT Domain.FarePolicy where
  fromTEntity entity = do
    let FarePolicyT {..} = entityVal entity
    perExtraKmRateList <- QExtraKmRate.findAll (fromKey organizationId)
    return $
      Domain.FarePolicy
        { id = Id id,
          organizationId = fromKey organizationId,
          baseFare = toRational <$> baseFare,
          nightShiftRate = toRational <$> nightShiftRate,
          ..
        }
  toTType Domain.FarePolicy {..} =
    FarePolicyT
      { id = getId id,
        organizationId = toKey organizationId,
        baseFare = fromRational <$> baseFare,
        nightShiftRate = fromRational <$> nightShiftRate,
        ..
      }
  toTEntity a =
    Entity (toKey a.id) $ toTType a
