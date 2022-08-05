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

Module      :  Storage.Tabular.RiderDetails
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Storage.Tabular.RiderDetails where

import Beckn.External.Encryption
import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import qualified Domain.Types.RiderDetails as Domain

mkPersist
  defaultSqlSettings
  [defaultQQ|
    RiderDetailsT sql=rider_details
      id Text
      mobileCountryCode Text
      mobileNumberEncrypted Text
      mobileNumberHash DbHash
      createdAt UTCTime
      updatedAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey RiderDetailsT where
  type DomainKey RiderDetailsT = Id Domain.RiderDetails
  fromKey (RiderDetailsTKey _id) = Id _id
  toKey (Id id) = RiderDetailsTKey id

instance TEntity RiderDetailsT Domain.RiderDetails where
  fromTEntity entity = do
    let RiderDetailsT {..} = entityVal entity
    return $
      Domain.RiderDetails
        { id = Id id,
          mobileNumber = EncryptedHashed (Encrypted mobileNumberEncrypted) mobileNumberHash,
          ..
        }
  toTType Domain.RiderDetails {..} =
    RiderDetailsT
      { id = getId id,
        mobileNumberEncrypted = unEncrypted mobileNumber.encrypted,
        mobileNumberHash = mobileNumber.hash,
        ..
      }
  toTEntity a =
    Entity (toKey a.id) $ toTType a
