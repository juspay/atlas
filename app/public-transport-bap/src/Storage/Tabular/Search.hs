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

Module      :  Storage.Tabular.Search
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Storage.Tabular.Search where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import qualified Domain.Types.Search as Domain

mkPersist
  defaultSqlSettings
  [defaultQQ|
    SearchT sql=search
      id Text
      lat Double
      lon Double
      requestorId Text
      createdAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey SearchT where
  type DomainKey SearchT = Id Domain.Search
  fromKey (SearchTKey _id) = Id _id
  toKey id = SearchTKey id.getId

instance TEntity SearchT Domain.Search where
  fromTEntity entity = do
    let SearchT {..} = entityVal entity
    return $
      Domain.Search
        { id = Id id,
          requestorId = Id requestorId,
          ..
        }
  toTType Domain.Search {..} =
    SearchT
      { id = id.getId,
        requestorId = requestorId.getId,
        ..
      }
  toTEntity a =
    Entity (toKey a.id) $ toTType a
