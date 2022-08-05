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

Module      :  Storage.Tabular.Quote.OneWayQuote
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Storage.Tabular.Quote.OneWayQuote where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import qualified Domain.Types.Quote as Domain
import qualified Domain.Types.Quote.OneWayQuote as Domain

-- FIXME quoteId should be QuoteTId, but I made it Text to avoid cyclic dependencies
mkPersist
  defaultSqlSettings
  [defaultQQ|
    OneWayQuoteT sql=one_way_quote
      quoteId Text
      distance Double
      distanceToNearestDriver Double
      Primary quoteId
      deriving Generic
    |]

instance TEntityKey OneWayQuoteT where
  type DomainKey OneWayQuoteT = Id Domain.Quote
  fromKey (OneWayQuoteTKey _id) = Id _id
  toKey (Id id) = OneWayQuoteTKey id

instance TEntity OneWayQuoteT Domain.OneWayQuote where
  fromTEntity entity = do
    let OneWayQuoteT {..} = entityVal entity
    return $
      Domain.OneWayQuote
        { quoteId = Id quoteId,
          ..
        }
  toTType Domain.OneWayQuote {..} =
    OneWayQuoteT
      { quoteId = getId quoteId,
        ..
      }
  toTEntity a =
    Entity (toKey a.quoteId) $ toTType a
