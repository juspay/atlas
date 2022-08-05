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

Module      :  Storage.Tabular.Organization
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Storage.Tabular.Organization where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import qualified Domain.Types.Organization as Domain

derivePersistField "Domain.OrganizationType"
derivePersistField "Domain.OrganizationDomain"
derivePersistField "Domain.Status"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    OrganizationT sql=organization
      id Text
      name Text
      description Text Maybe
      shortId Text
      uniqueKeyId Text
      mobileNumber Text Maybe
      mobileCountryCode Text Maybe
      gstin Text Maybe
      orgType Domain.OrganizationType sql=type
      domain Domain.OrganizationDomain Maybe
      fromTime UTCTime Maybe
      toTime UTCTime Maybe
      headCount Int Maybe
      status Domain.Status
      verified Bool
      enabled Bool
      createdAt UTCTime
      updatedAt UTCTime
      info Text Maybe
      Primary id
      Unique OrganizationShortId
      deriving Generic
    |]

instance TEntityKey OrganizationT where
  type DomainKey OrganizationT = Id Domain.Organization
  fromKey (OrganizationTKey _id) = Id _id
  toKey (Id id) = OrganizationTKey id

instance TEntity OrganizationT Domain.Organization where
  fromTEntity entity = do
    let OrganizationT {..} = entityVal entity
    return $
      Domain.Organization
        { id = Id id,
          shortId = ShortId shortId,
          _type = orgType,
          ..
        }
  toTType Domain.Organization {..} =
    OrganizationT
      { id = getId id,
        shortId = getShortId shortId,
        orgType = _type,
        ..
      }
  toTEntity a =
    Entity (toKey a.id) $ toTType a
