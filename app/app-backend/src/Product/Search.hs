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

Module      :  Product.Search
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Product.Search where

import App.Types
import qualified Beckn.Storage.Esqueleto as Esq
import qualified Beckn.Storage.Queries.BecknRequest as QBR
import Beckn.Types.Common hiding (id)
import Beckn.Types.Core.Ack
import qualified Beckn.Types.Core.Taxi.API.OnSearch as OnSearch
import Beckn.Types.Id
import Beckn.Utils.Logging
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import qualified Core.ACL.Metro.Search as MetroACL
import qualified Core.ACL.OnSearch as TaxiACL
import qualified Core.ACL.Search as TaxiACL
import Data.Aeson (encode)
import qualified Domain.Action.Beckn.OnSearch as DOnSearch
import qualified Domain.Action.UI.Search.OneWay as DOneWaySearch
import qualified Domain.Action.UI.Search.Rental as DRentalSearch
import qualified Domain.Types.Person as Person
import EulerHS.Prelude hiding (id, state)
import qualified ExternalAPI.Flow as ExternalAPI
import qualified Types.API.Search as API
import Utils.Common

search :: Id Person.Person -> API.SearchReq -> FlowHandler API.SearchRes
search personId = \case
  API.OneWaySearch req -> oneWaySearch personId req
  API.RentalSearch req -> rentalSearch personId req

oneWaySearch :: Id Person.Person -> API.OneWaySearchReq -> FlowHandler API.SearchRes
oneWaySearch personId req = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  (searchRes, dSearchReq) <- DOneWaySearch.search personId req
  fork "search cabs" . withRetry $ do
    becknTaxiReq <- TaxiACL.buildOneWaySearchReq dSearchReq
    void $ ExternalAPI.search becknTaxiReq
  fork "search metro" . withRetry $ do
    becknMetroReq <- MetroACL.buildSearchReq dSearchReq
    ExternalAPI.searchMetro becknMetroReq
  fork "search public-transport" $ DOneWaySearch.sendPublicTransportSearchRequest personId dSearchReq
  return searchRes

rentalSearch :: Id Person.Person -> API.RentalSearchReq -> FlowHandler API.SearchRes
rentalSearch personId req = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  (searchRes, dSearchReq) <- DRentalSearch.search personId req
  fork "search rental" . withRetry $ do
    -- do we need fork here?
    becknReq <- TaxiACL.buildRentalSearchReq dSearchReq
    void $ ExternalAPI.search becknReq
  pure searchRes

searchCb ::
  SignatureAuthResult ->
  SignatureAuthResult ->
  OnSearch.OnSearchReq ->
  FlowHandler AckResponse
searchCb (SignatureAuthResult signPayload _) _ req = withFlowHandlerBecknAPI . withTransactionIdLogTag req $ do
  mbDOnSearchReq <- TaxiACL.buildOnSearchReq req
  DOnSearch.searchCb req.context.message_id mbDOnSearchReq
  Esq.runTransaction $
    QBR.logBecknRequest (show $ encode req) (show $ signPayload.signature)
  pure Ack
