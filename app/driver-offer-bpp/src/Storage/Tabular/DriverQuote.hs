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

Module      :  Storage.Tabular.DriverQuote
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Storage.Tabular.DriverQuote where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Common (Meters (..), Seconds (..))
import Beckn.Types.Id
import qualified Domain.Types.DriverQuote as Domain
import qualified Domain.Types.Vehicle.Variant as Variant
import Storage.Tabular.Person (PersonTId)
import qualified Storage.Tabular.SearchRequest as SReq

derivePersistField "Domain.DriverQuoteStatus"
derivePersistField "Variant.Variant"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    DriverQuoteT sql=driver_quote
      id Text
      status Domain.DriverQuoteStatus
      searchRequestId SReq.SearchRequestTId
      driverId PersonTId
      baseFare Double
      extraFareSelected Double Maybe
      vehicleVariant Variant.Variant
      distanceToPickup Int
      durationToPickup Double
      validTill UTCTime
      createdAt UTCTime
      updatedAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey DriverQuoteT where
  type DomainKey DriverQuoteT = Id Domain.DriverQuote
  fromKey (DriverQuoteTKey _id) = Id _id
  toKey (Id id) = DriverQuoteTKey id

instance TEntity DriverQuoteT Domain.DriverQuote where
  fromTEntity entity = do
    let DriverQuoteT {..} = entityVal entity
    return $
      Domain.DriverQuote
        { id = Id id,
          searchRequestId = fromKey searchRequestId,
          driverId = fromKey driverId,
          distanceToPickup = Meters distanceToPickup,
          durationToPickup = Seconds $ floor durationToPickup,
          ..
        }
  toTType Domain.DriverQuote {..} =
    DriverQuoteT
      { id = getId id,
        searchRequestId = toKey searchRequestId,
        driverId = toKey driverId,
        distanceToPickup = getMeters distanceToPickup,
        durationToPickup = fromIntegral $ getSeconds durationToPickup,
        ..
      }
  toTEntity a =
    Entity (toKey a.id) $ toTType a
