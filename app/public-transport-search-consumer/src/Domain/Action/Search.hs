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

Module      :  Domain.Action.Search
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Domain.Action.Search where

import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Streaming.Kafka.Topic.PublicTransportSearch (PublicTransportSearch)
import Beckn.Types.Common hiding (id)
import Beckn.Types.Id
import Beckn.Types.MapSearch (LatLong)
import Data.Time.Clock (addUTCTime)
import qualified Domain.Types.Search as DSearch
import qualified Storage.Queries.Search as QSearch

type SearchReq = PublicTransportSearch

data SearchMessage = SearchMessage
  { searchId :: Id DSearch.Search,
    gps :: LatLong,
    fromDate :: UTCTime,
    toDate :: UTCTime
  }

search :: EsqDBFlow m r => SearchReq -> m SearchMessage
search req = do
  now <- getCurrentTime
  let searchRequest = makeSearchRequest now
  _ <- Esq.runTransaction $ QSearch.create searchRequest
  let searchMessage =
        SearchMessage
          { searchId = searchRequest.id,
            gps = req.gps,
            fromDate = now,
            toDate = getToDate now
          }
  return searchMessage
  where
    getToDate = addUTCTime 7200 -- 2 hours
    makeSearchRequest now =
      DSearch.Search
        { id = Id req.id,
          lat = req.gps.lat,
          lon = req.gps.lon,
          requestorId = Id req.requestorId,
          createdAt = now
        }
