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

Module      :  Storage.Tabular.Quote.RentalQuote
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Storage.Tabular.Quote.RentalQuote where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import qualified Domain.Types.Quote as Domain
import qualified Domain.Types.Quote.RentalQuote as Domain
import Storage.Tabular.RentalFarePolicy (RentalFarePolicyTId)

-- FIXME quoteId should be QuoteTId, but I made it Text to avoid cyclic dependencies
mkPersist
  defaultSqlSettings
  [defaultQQ|
    RentalQuoteT sql=rental_quote
      quoteId Text
      rentalFarePolicyId RentalFarePolicyTId
      Primary quoteId
      deriving Generic
    |]

instance TEntityKey RentalQuoteT where
  type DomainKey RentalQuoteT = Id Domain.Quote
  fromKey (RentalQuoteTKey _id) = Id _id
  toKey (Id id) = RentalQuoteTKey id

instance TEntity RentalQuoteT Domain.RentalQuote where
  fromTEntity entity = do
    let RentalQuoteT {..} = entityVal entity
    return $
      Domain.RentalQuote
        { quoteId = Id quoteId,
          rentalFarePolicyId = fromKey rentalFarePolicyId
        }
  toTType Domain.RentalQuote {..} =
    RentalQuoteT
      { quoteId = getId quoteId,
        rentalFarePolicyId = toKey rentalFarePolicyId
      }
  toTEntity a =
    Entity (toKey a.quoteId) $ toTType a
