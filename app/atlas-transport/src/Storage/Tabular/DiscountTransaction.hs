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

Module      :  Storage.Tabular.DiscountTransaction
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Storage.Tabular.DiscountTransaction where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Amount
import Beckn.Types.Id
import qualified Domain.Types.DiscountTransaction as Domain
import Domain.Types.RideBooking (RideBooking)
import Storage.Tabular.Organization (OrganizationTId)
import Storage.Tabular.RideBooking (RideBookingTId)

mkPersist
  defaultSqlSettings
  [defaultQQ|
    DiscountTransactionT sql=discount_transaction
      rideBookingId RideBookingTId sql=ride_booking_id
      organizationId OrganizationTId
      discount Amount
      createdAt UTCTime
      Primary rideBookingId
      deriving Generic
    |]

instance TEntityKey DiscountTransactionT where
  type DomainKey DiscountTransactionT = Id RideBooking
  fromKey (DiscountTransactionTKey _id) = fromKey _id
  toKey id = DiscountTransactionTKey $ toKey id

instance TEntity DiscountTransactionT Domain.DiscountTransaction where
  fromTEntity entity = do
    let DiscountTransactionT {..} = entityVal entity
    return $
      Domain.DiscountTransaction
        { rideBookingId = fromKey rideBookingId,
          organizationId = fromKey organizationId,
          ..
        }
  toTType Domain.DiscountTransaction {..} =
    DiscountTransactionT
      { rideBookingId = toKey rideBookingId,
        organizationId = toKey organizationId,
        ..
      }
  toTEntity a =
    Entity (toKey a.rideBookingId) $ toTType a
