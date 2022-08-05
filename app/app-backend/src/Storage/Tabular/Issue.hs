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

Module      :  Storage.Tabular.Issue
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Storage.Tabular.Issue where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import qualified Domain.Types.Issue as Domain
import qualified Storage.Tabular.Person as SPerson
import qualified Storage.Tabular.Quote as SQuote

--FIXME: rideBookingId SQuote.QuoteTId Maybe
mkPersist
  defaultSqlSettings
  [defaultQQ|
    IssueT sql=issue
      id Text
      customerId SPerson.PersonTId
      rideBookingId SQuote.QuoteTId Maybe
      contactEmail Text Maybe
      reason Text
      description Text
      createdAt UTCTime
      updatedAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey IssueT where
  type DomainKey IssueT = Id Domain.Issue
  fromKey (IssueTKey _id) = Id _id
  toKey (Id id) = IssueTKey id

instance TEntity IssueT Domain.Issue where
  fromTEntity entity = do
    let IssueT {..} = entityVal entity
    return $
      Domain.Issue
        { id = Id id,
          customerId = fromKey customerId,
          rideBookingId = fromKey <$> rideBookingId,
          ..
        }
  toTType Domain.Issue {..} =
    IssueT
      { id = getId id,
        customerId = toKey customerId,
        rideBookingId = toKey <$> rideBookingId,
        ..
      }
  toTEntity a =
    Entity (toKey a.id) $ toTType a
