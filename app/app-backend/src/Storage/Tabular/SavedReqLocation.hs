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

Module      :  Storage.Tabular.SavedReqLocation
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Storage.Tabular.SavedReqLocation where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import qualified Domain.Types.SavedReqLocation as Domain
import qualified Storage.Tabular.Person as Person

mkPersist
  defaultSqlSettings
  [defaultQQ|
    SavedReqLocationT sql=saved_location
      id Text
      lat Double
      lon Double
      street Text Maybe
      door Text Maybe
      city Text Maybe
      state Text Maybe
      country Text Maybe
      building Text Maybe
      areaCode Text Maybe
      area Text Maybe
      createdAt UTCTime
      updatedAt UTCTime
      tag  Text
      riderId Person.PersonTId
      Primary id
      deriving Generic
    |]

instance TEntityKey SavedReqLocationT where
  type DomainKey SavedReqLocationT = Id Domain.SavedReqLocation
  fromKey (SavedReqLocationTKey _id) = Id _id
  toKey (Id id) = SavedReqLocationTKey id

instance TEntity SavedReqLocationT Domain.SavedReqLocation where
  fromTEntity entity = do
    let SavedReqLocationT {..} = entityVal entity
    return $
      Domain.SavedReqLocation
        { id = Id id,
          riderId = fromKey riderId,
          ..
        }
  toTType Domain.SavedReqLocation {..} =
    SavedReqLocationT
      { id = getId id,
        riderId = toKey riderId,
        ..
      }
  toTEntity a =
    Entity (toKey a.id) $ toTType a
