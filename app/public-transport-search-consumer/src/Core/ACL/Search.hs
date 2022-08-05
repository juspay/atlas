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

module Core.ACL.Search where

import Beckn.Prelude
import Beckn.Types.Core.ReqTypes
import Beckn.Types.Id
import Beckn.Types.MapSearch (LatLong (..))
import Beckn.Utils.Common
import Core.Context
import qualified Core.Spec.Common.Context as Context
import Core.Spec.Common.Gps
import Core.Spec.Search
import qualified Domain.Action.Search as DSearch

buildSearchReq ::
  ( MonadFlow m,
    MonadReader r m,
    HasField "bapId" r Text,
    HasField "bapURI" r BaseUrl
  ) =>
  DSearch.SearchMessage ->
  m (BecknReq SearchMessage)
buildSearchReq msg = do
  let txnId = getId (msg.searchId)
  bapId <- asks (.bapId)
  bapURI <- asks (.bapURI)
  context <- buildContext Context.SEARCH txnId bapId bapURI Nothing Nothing
  let intent = mkIntent msg
  pure (BecknReq context $ SearchMessage intent)

mkIntent :: DSearch.SearchMessage -> Intent
mkIntent msg = do
  Intent
    { fulfillment =
        Fulfillment
          { start =
              StartInfo
                { location =
                    LocationGps
                      { gps = toGps msg.gps
                      },
                  time =
                    StartTime $
                      TimeRange
                        { start = msg.fromDate,
                          end = msg.toDate
                        }
                },
            end =
              EndInfo $
                LocationGps
                  { gps = toGps msg.gps
                  }
          }
    }
  where
    toGps LatLong {..} = Gps {..}
