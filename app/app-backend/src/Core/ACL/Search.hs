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

Module      :  Core.ACL.Search
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Core.ACL.Search (buildRentalSearchReq, buildOneWaySearchReq) where

import Beckn.Types.Common
import qualified Beckn.Types.Core.Context as Context
import Beckn.Types.Core.ReqTypes
import qualified Beckn.Types.Core.Taxi.Search as Search
import Beckn.Types.Id
import qualified Domain.Action.UI.Search.OneWay as DOneWaySearch
import qualified Domain.Action.UI.Search.Rental as DRentalSearch
import qualified Domain.Types.SearchRequest as DSearchReq
import EulerHS.Prelude hiding (state)
import ExternalAPI.Flow
import qualified Types.API.Search as API
import Utils.Common

buildOneWaySearchReq ::
  (HasFlowEnv m r ["bapSelfIds" ::: BAPs Text, "bapSelfURIs" ::: BAPs BaseUrl]) =>
  DOneWaySearch.DSearchReq ->
  m (BecknReq Search.SearchMessage)
buildOneWaySearchReq DOneWaySearch.DSearchReq {..} = buildSearchReq origin (Just destination) searchId now

buildRentalSearchReq ::
  (HasFlowEnv m r ["bapSelfIds" ::: BAPs Text, "bapSelfURIs" ::: BAPs BaseUrl]) =>
  DRentalSearch.DSearchReq ->
  m (BecknReq Search.SearchMessage)
buildRentalSearchReq DRentalSearch.DSearchReq {..} = buildSearchReq origin Nothing searchId startTime

buildSearchReq ::
  (HasFlowEnv m r ["bapSelfIds" ::: BAPs Text, "bapSelfURIs" ::: BAPs BaseUrl]) =>
  API.SearchReqLocation ->
  Maybe API.SearchReqLocation ->
  Id DSearchReq.SearchRequest ->
  UTCTime ->
  m (BecknReq Search.SearchMessage)
buildSearchReq origin mbDestination searchId startTime = do
  let messageId = getId searchId
  bapURIs <- asks (.bapSelfURIs)
  bapIDs <- asks (.bapSelfIds)
  context <- buildTaxiContext Context.SEARCH messageId Nothing bapIDs.cabs bapURIs.cabs Nothing Nothing
  let intent = mkIntent origin mbDestination startTime
  pure $ BecknReq context $ Search.SearchMessage intent

mkIntent ::
  API.SearchReqLocation ->
  Maybe API.SearchReqLocation ->
  UTCTime ->
  Search.Intent
mkIntent origin mbDestination startTime = do
  let startLocation =
        Search.StartInfo
          { location = mkLocation origin,
            time = Search.Time startTime
          }
      mkStopInfo destination =
        Search.StopInfo
          { location = mkLocation destination
          }
      mbEndLocation = mkStopInfo <$> mbDestination

      fulfillment =
        Search.FulFillmentInfo
          { start = startLocation,
            end = mbEndLocation
          }
  Search.Intent
    { ..
    }
  where
    mkLocation info =
      Search.Location
        { gps =
            Search.Gps
              { lat = info.gps.lat,
                lon = info.gps.lon
              },
          address = do
            let API.SearchReqAddress {..} = info.address
            Search.Address
              { area_code = areaCode,
                ..
              }
        }
