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

Module      :  Storage.Queries.SavedReqLocation
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Storage.Queries.SavedReqLocation where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.Person (Person)
import Domain.Types.SavedReqLocation
import Storage.Tabular.SavedReqLocation

create :: SavedReqLocation -> SqlDB ()
create = create'

findAllByRiderId :: Transactionable m => Id Person -> m [SavedReqLocation]
findAllByRiderId perId =
  Esq.findAll $ do
    saveReqLocation <- from $ table @SavedReqLocationT
    where_ $
      saveReqLocation ^. SavedReqLocationRiderId ==. val (toKey perId)
    return saveReqLocation

deleteByRiderIdAndTag :: Id Person -> Text -> SqlDB ()
deleteByRiderIdAndTag perId addressTag = do
  delete' $ do
    saveReqLocation <- from $ table @SavedReqLocationT
    where_ $
      (saveReqLocation ^. SavedReqLocationRiderId ==. val (toKey perId))
        &&. (saveReqLocation ^. SavedReqLocationTag ==. val addressTag)

findAllByRiderIdAndTag :: Transactionable m => Id Person -> Text -> m [SavedReqLocation]
findAllByRiderIdAndTag perId addressTag =
  Esq.findAll $ do
    saveReqLocation <- from $ table @SavedReqLocationT
    where_ $
      (saveReqLocation ^. SavedReqLocationRiderId ==. val (toKey perId))
        &&. (saveReqLocation ^. SavedReqLocationTag ==. val addressTag)
    return saveReqLocation
