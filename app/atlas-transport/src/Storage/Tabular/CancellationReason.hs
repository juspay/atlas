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

Module      :  Storage.Tabular.CancellationReason
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Storage.Tabular.CancellationReason where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import qualified Domain.Types.CancellationReason as Domain

mkPersist
  defaultSqlSettings
  [defaultQQ|
    CancellationReasonT sql=cancellation_reason
      reasonCode Text
      description Text
      enabled Bool
      Primary reasonCode
      deriving Generic
    |]

instance TEntityKey CancellationReasonT where
  type DomainKey CancellationReasonT = Domain.CancellationReasonCode
  fromKey (CancellationReasonTKey _id) = Domain.CancellationReasonCode _id
  toKey (Domain.CancellationReasonCode id) = CancellationReasonTKey id

instance TEntity CancellationReasonT Domain.CancellationReason where
  fromTEntity entity = do
    let CancellationReasonT {..} = entityVal entity
    return $
      Domain.CancellationReason
        { reasonCode = Domain.CancellationReasonCode reasonCode,
          ..
        }
  toTType Domain.CancellationReason {..} =
    CancellationReasonT
      { reasonCode = let (Domain.CancellationReasonCode rc) = reasonCode in rc,
        ..
      }
  toTEntity a =
    Entity (toKey a.reasonCode) $ toTType a
