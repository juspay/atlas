{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE UndecidableInstances #-}


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

Module      :  Domain.Types.RiderDetails
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Domain.Types.RiderDetails where

import Beckn.External.Encryption
import Beckn.Types.Id
import Data.Time
import EulerHS.Prelude hiding (id)

data RiderDetailsE e = RiderDetails
  { id :: Id RiderDetails,
    mobileCountryCode :: Text,
    mobileNumber :: EncryptedHashedField e Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic)

type RiderDetails = RiderDetailsE 'AsEncrypted

type RiderDetailsDecrypted = RiderDetailsE 'AsUnencrypted

instance EncryptedItem RiderDetails where
  type Unencrypted RiderDetails = (RiderDetailsDecrypted, HashSalt)
  encryptItem (RiderDetails {..}, salt) = do
    mobileNumber_ <- encryptItem (mobileNumber, salt)
    return RiderDetails {mobileNumber = mobileNumber_, ..}
  decryptItem RiderDetails {..} = do
    mobileNumber_ <- fst <$> decryptItem mobileNumber
    return (RiderDetails {mobileNumber = mobileNumber_, ..}, "")

instance EncryptedItem' RiderDetails where
  type UnencryptedItem RiderDetails = RiderDetailsDecrypted
  toUnencrypted a salt = (a, salt)
  fromUnencrypted a = fst a
