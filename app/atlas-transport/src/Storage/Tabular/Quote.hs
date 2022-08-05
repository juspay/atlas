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

Module      :  Storage.Tabular.Quote
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Storage.Tabular.Quote where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Amount
import Beckn.Types.Id
import qualified Domain.Types.FareProduct as Domain
import qualified Domain.Types.Quote as Domain
import qualified Domain.Types.Vehicle as Vehicle
import qualified Storage.Queries.Quote.OneWayQuote as QOneWayQuote
import qualified Storage.Queries.Quote.RentalQuote as QRentalQuote
import qualified Storage.Queries.RentalFarePolicy as QRentalFarePolicy
import Storage.Tabular.FareProduct ()
import Storage.Tabular.Organization (OrganizationTId)
import Storage.Tabular.Products (ProductsTId)
import Storage.Tabular.SearchRequest (SearchRequestTId)
import Storage.Tabular.Vehicle ()
import Types.Error
import Utils.Common hiding (id)

mkPersist
  defaultSqlSettings
  [defaultQQ|
    QuoteT sql=quote
      id Text
      fareProductType Domain.FareProductType
      requestId SearchRequestTId
      productId ProductsTId
      estimatedFare Amount
      discount Amount Maybe
      estimatedTotalFare Amount
      providerId OrganizationTId
      vehicleVariant Vehicle.Variant
      createdAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey QuoteT where
  type DomainKey QuoteT = Id Domain.Quote
  fromKey (QuoteTKey _id) = Id _id
  toKey (Id id) = QuoteTKey id

instance TEntity QuoteT Domain.Quote where
  fromTEntity entity = do
    let QuoteT {..} = entityVal entity
    quoteDetails <- case fareProductType of
      Domain.RENTAL -> do
        rentalQuote <- QRentalQuote.findByQuoteId (Id id) >>= fromMaybeM (QuoteDoesNotExist id)
        rentalFarePolicy <-
          QRentalFarePolicy.findById rentalQuote.rentalFarePolicyId
            >>= fromMaybeM NoRentalFarePolicy
        return $ Domain.mkRentalQuoteDetails rentalFarePolicy
      Domain.ONE_WAY -> do
        oneWayQuoteEntity <- QOneWayQuote.findByQuoteId (Id id) >>= fromMaybeM (QuoteDoesNotExist id)
        return . Domain.OneWayDetails $
          Domain.OneWayQuoteDetails
            { distance = oneWayQuoteEntity.distance,
              distanceToNearestDriver = oneWayQuoteEntity.distance,
              ..
            }
    pure
      Domain.Quote
        { id = Id id,
          requestId = fromKey requestId,
          productId = fromKey productId,
          providerId = fromKey providerId,
          ..
        }

  toTType Domain.Quote {..} =
    QuoteT
      { id = getId id,
        fareProductType = Domain.getFareProductType quoteDetails,
        requestId = toKey requestId,
        productId = toKey productId,
        providerId = toKey providerId,
        ..
      }

  toTEntity quote =
    Entity (toKey quote.id) $ toTType quote
