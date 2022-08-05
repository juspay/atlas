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

Module      :  Domain.Action.Beckn.OnSearch
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Domain.Action.Beckn.OnSearch where

import App.Types
import qualified Beckn.Storage.Esqueleto as DB
import Beckn.Types.Amount
import Beckn.Types.Common hiding (id)
import Beckn.Types.Id
import qualified Domain.Types.Quote as DQuote
import qualified Domain.Types.SearchRequest as DSearchReq
import EulerHS.Prelude hiding (id, state)
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.SearchRequest as QSearchReq
import qualified Tools.Metrics as Metrics
import Types.Error
import Utils.Common

data DOnSearchReq = DOnSearchReq
  { requestId :: Id DSearchReq.SearchRequest,
    providerInfo :: ProviderInfo,
    quotesInfo :: [QuoteInfo]
  }

data ProviderInfo = ProviderInfo
  { providerId :: Text,
    name :: Text,
    url :: BaseUrl,
    mobileNumber :: Text,
    ridesCompleted :: Int
  }

data QuoteInfo = QuoteInfo
  { bppQuoteId :: Id DQuote.BPPQuote,
    vehicleVariant :: Text,
    estimatedFare :: Amount,
    discount :: Maybe Amount,
    estimatedTotalFare :: Amount,
    quoteDetails :: DQuote.QuoteDetails,
    descriptions :: [Text]
  }

searchCb ::
  Text ->
  Maybe DOnSearchReq ->
  Flow ()
searchCb transactionId mbReq = do
  Metrics.finishSearchMetrics transactionId -- move it to api handler or acl?
  whenJust mbReq searchCbService

searchCbService ::
  DOnSearchReq ->
  Flow ()
searchCbService DOnSearchReq {..} = do
  _searchRequest <- QSearchReq.findById requestId >>= fromMaybeM (SearchRequestDoesNotExist requestId.getId)
  now <- getCurrentTime
  quotes <- traverse (buildQuote requestId providerInfo now) quotesInfo
  whenM (duplicateCheckCond (quotes <&> (.bppQuoteId)) requestId providerInfo.providerId) $
    throwError $ InvalidRequest "Duplicate OnSearch request"
  DB.runTransaction $ traverse_ QQuote.create quotes
  where
    duplicateCheckCond :: EsqDBFlow m r => [Id DQuote.BPPQuote] -> Id DSearchReq.SearchRequest -> Text -> m Bool
    duplicateCheckCond [] _ _ = return False
    duplicateCheckCond (bppQuoteId_ : _) txnId_ bppId_ =
      isJust <$> QQuote.findByTxnIdAndBppIdAndQuoteId txnId_ bppId_ bppQuoteId_

buildQuote ::
  MonadFlow m =>
  Id DSearchReq.SearchRequest ->
  ProviderInfo ->
  UTCTime ->
  QuoteInfo ->
  m DQuote.Quote
buildQuote requestId providerInfo now QuoteInfo {..} = do
  uid <- generateGUID
  quoteTerms <- traverse buildQuoteTerms descriptions
  pure
    DQuote.Quote
      { id = uid,
        providerMobileNumber = providerInfo.mobileNumber,
        providerName = providerInfo.name,
        providerCompletedRidesCount = providerInfo.ridesCompleted,
        providerId = providerInfo.providerId,
        providerUrl = providerInfo.url,
        createdAt = now,
        ..
      }

buildQuoteTerms ::
  MonadFlow m =>
  Text ->
  m DQuote.QuoteTerms
buildQuoteTerms description = do
  uid <- generateGUID
  return
    DQuote.QuoteTerms
      { id = uid,
        ..
      }
