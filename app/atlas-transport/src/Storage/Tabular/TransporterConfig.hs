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

Module      :  Storage.Tabular.TransporterConfig
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Storage.Tabular.TransporterConfig where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import qualified Domain.Types.TransporterConfig as Domain
import Storage.Tabular.Organization (OrganizationTId)

derivePersistField "Domain.ConfigKey"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    TransporterConfigT sql=transporter_config
      id Text
      transporterId OrganizationTId
      configKey Domain.ConfigKey sql=key
      value Text
      createdAt UTCTime
      updatedAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey TransporterConfigT where
  type DomainKey TransporterConfigT = Id Domain.TransporterParameter
  fromKey (TransporterConfigTKey _id) = Id _id
  toKey (Id id) = TransporterConfigTKey id

instance TEntity TransporterConfigT Domain.TransporterConfig where
  fromTEntity entity = do
    let TransporterConfigT {..} = entityVal entity
    return $
      Domain.TransporterConfig
        { id = Id id,
          transporterId = fromKey transporterId,
          key = configKey,
          ..
        }
  toTType Domain.TransporterConfig {..} =
    TransporterConfigT
      { id = getId id,
        transporterId = toKey transporterId,
        configKey = key,
        ..
      }
  toTEntity a =
    Entity (toKey a.id) $ toTType a
