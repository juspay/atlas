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

Module      :  Storage.Queries.RideRequest
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Storage.Queries.RideRequest where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.Organization
import Domain.Types.RideRequest
import Storage.Tabular.RideRequest

create :: RideRequest -> SqlDB ()
create = Esq.create'

fetchOldest :: Transactionable m => ShortId Organization -> Integer -> m [RideRequest]
fetchOldest shortId limit' = do
  let limitVal = fromIntegral limit'
  Esq.findAll $ do
    rideRequest <- from $ table @RideRequestT
    where_ $ rideRequest ^. RideRequestShortOrgId ==. val (getShortId shortId)
    orderBy [asc $ rideRequest ^. RideRequestCreatedAt]
    limit limitVal
    return rideRequest

removeRequest :: Id RideRequest -> SqlDB ()
removeRequest = Esq.deleteByKey' @RideRequestT
