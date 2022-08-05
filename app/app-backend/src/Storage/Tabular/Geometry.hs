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

Module      :  Storage.Tabular.Geometry
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Storage.Tabular.Geometry where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import qualified Domain.Types.Geometry as Domain

mkPersist
  defaultSqlSettings
  [defaultQQ|
    GeometryT sql=geometry
      id Int
      region Text
      Primary id
      deriving Generic
    |]

instance TEntityKey GeometryT where
  type DomainKey GeometryT = Int
  fromKey (GeometryTKey _id) = _id
  toKey id = GeometryTKey id

instance TEntity GeometryT Domain.Geometry where
  fromTEntity entity = do
    let GeometryT {..} = entityVal entity
    return $
      Domain.Geometry
        { ..
        }
  toTType Domain.Geometry {..} =
    GeometryT
      { ..
      }
  toTEntity a =
    Entity (toKey a.id) $ toTType a
