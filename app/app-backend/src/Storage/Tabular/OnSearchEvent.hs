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

Module      :  Storage.Tabular.OnSearchEvent
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Storage.Tabular.OnSearchEvent where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import qualified Domain.Types.OnSearchEvent as Domain

mkPersist
  defaultSqlSettings
  [defaultQQ|
    OnSearchEventT sql=on_search_event
      id Text
      bppId Text
      messageId Text
      errorCode Text Maybe
      errorType Text Maybe
      errorMessage Text Maybe
      createdAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey OnSearchEventT where
  type DomainKey OnSearchEventT = Id Domain.OnSearchEvent
  fromKey (OnSearchEventTKey _id) = Id _id
  toKey (Id id) = OnSearchEventTKey id

instance TEntity OnSearchEventT Domain.OnSearchEvent where
  fromTEntity entity = do
    let OnSearchEventT {..} = entityVal entity
    return $
      Domain.OnSearchEvent
        { id = Id id,
          ..
        }
  toTType Domain.OnSearchEvent {..} =
    OnSearchEventT
      { id = getId id,
        ..
      }
  toTEntity a =
    Entity (toKey a.id) $ toTType a
