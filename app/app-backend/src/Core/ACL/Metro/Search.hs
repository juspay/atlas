{-# LANGUAGE OverloadedLabels #-}


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

Module      :  Core.ACL.Metro.Search
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Core.ACL.Metro.Search (buildSearchReq) where

import Beckn.Types.Common
import qualified Beckn.Types.Core.Context as Context
import qualified Beckn.Types.Core.Metro.API.Search as Search
import qualified Beckn.Types.Core.Metro.Search as Search
import Beckn.Types.Core.ReqTypes
import Beckn.Types.Id
import Control.Lens ((?~))
import qualified Domain.Action.UI.Search.OneWay as DSearch
import EulerHS.Prelude hiding (state)
import ExternalAPI.Flow
import Product.MetroOffer (buildContextMetro)
import qualified Types.API.Search as API
import Utils.Common

buildSearchReq ::
  (HasFlowEnv m r ["bapSelfIds" ::: BAPs Text, "bapSelfURIs" ::: BAPs BaseUrl]) =>
  DSearch.DSearchReq ->
  m (BecknReq Search.SearchIntent)
buildSearchReq req@DSearch.DSearchReq {..} = do
  bapURIs <- asks (.bapSelfURIs)
  bapIDs <- asks (.bapSelfIds)
  let messageId = getId searchId
  context <- buildContextMetro Context.SEARCH messageId bapIDs.metro bapURIs.metro
  let intent = mkIntent req
  pure $ BecknReq context $ Search.SearchIntent intent

mkIntent :: DSearch.DSearchReq -> Search.Intent
mkIntent req = do
  let from = stopToLoc req.origin
  let to = stopToLoc req.destination
  Search.emptyIntent
    & #fulfillment
      ?~ ( Search.emptyFulFillmentInfo
             & #start
               ?~ Search.LocationAndTime
                 { location = Just from,
                   time = Nothing
                 }
             & #end
               ?~ Search.LocationAndTime
                 { location = Just to,
                   time = Nothing
                 }
         )
  where
    stopToLoc API.SearchReqLocation {gps} = do
      let gps' = Search.Gps gps.lat gps.lon
      Search.emptyLocation & #gps ?~ gps'
