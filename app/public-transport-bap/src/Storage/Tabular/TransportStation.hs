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

Module      :  Storage.Tabular.TransportStation
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Storage.Tabular.TransportStation where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import qualified Domain.Types.TransportStation as Domain

mkPersist
  defaultSqlSettings
  [defaultQQ|
    TransportStationT sql=transport_station
      id Text
      name Text
      stationCode Text
      lat Double
      lon Double
      Primary id
      deriving Generic
    |]

instance TEntityKey TransportStationT where
  type DomainKey TransportStationT = Id Domain.TransportStation
  fromKey (TransportStationTKey _id) = Id _id
  toKey id = TransportStationTKey id.getId

instance TEntity TransportStationT Domain.TransportStation where
  fromTEntity entity = do
    let TransportStationT {..} = entityVal entity
    return $
      Domain.TransportStation
        { id = Id id,
          ..
        }
  toTType Domain.TransportStation {..} =
    TransportStationT
      { id = id.getId,
        ..
      }
  toTEntity a =
    Entity (toKey (Id a.stationCode)) $ toTType a
