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

Module      :  Storage.Tabular.Person
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Storage.Tabular.Person where

import Beckn.External.Encryption (DbHash, Encrypted (..), EncryptedHashed (..))
import Beckn.External.FCM.Types (FCMRecipientToken)
import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import qualified Domain.Types.Person as Domain
import Storage.Tabular.Organization (OrganizationTId)

derivePersistField "Domain.Role"
derivePersistField "Domain.Gender"
derivePersistField "Domain.IdentifierType"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    PersonT sql=person
      id Text
      firstName Text
      middleName Text Maybe
      lastName Text Maybe
      role Domain.Role
      gender Domain.Gender
      identifierType Domain.IdentifierType
      email Text Maybe
      mobileNumberEncrypted Text Maybe
      mobileNumberHash DbHash Maybe
      mobileCountryCode Text Maybe
      passwordHash DbHash Maybe
      identifier Text Maybe
      rating Double Maybe
      isNew Bool
      udf1 Text Maybe
      udf2 Text Maybe
      organizationId OrganizationTId Maybe
      deviceToken FCMRecipientToken Maybe
      description Text Maybe
      createdAt UTCTime
      updatedAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey PersonT where
  type DomainKey PersonT = Id Domain.Person
  fromKey (PersonTKey _id) = Id _id
  toKey (Id id) = PersonTKey id

instance TEntity PersonT Domain.Person where
  fromTEntity entity = do
    let PersonT {..} = entityVal entity
    return $
      Domain.Person
        { id = Id id,
          mobileNumber = EncryptedHashed <$> (Encrypted <$> mobileNumberEncrypted) <*> mobileNumberHash,
          organizationId = fromKey <$> organizationId,
          ..
        }
  toTType Domain.Person {..} =
    PersonT
      { id = getId id,
        mobileNumberEncrypted = mobileNumber <&> unEncrypted . (.encrypted),
        mobileNumberHash = mobileNumber <&> (.hash),
        organizationId = toKey <$> organizationId,
        ..
      }
  toTEntity a =
    Entity (toKey a.id) $ toTType a
