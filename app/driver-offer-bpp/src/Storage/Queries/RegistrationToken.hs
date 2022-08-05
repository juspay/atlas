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

Module      :  Storage.Queries.RegistrationToken
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Storage.Queries.RegistrationToken where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.Person
import Domain.Types.RegistrationToken
import Storage.Tabular.RegistrationToken
import Utils.Common

create :: RegistrationToken -> SqlDB ()
create = Esq.create'

findById :: Transactionable m => Id RegistrationToken -> m (Maybe RegistrationToken)
findById = Esq.findById

setVerified :: Id RegistrationToken -> SqlDB ()
setVerified rtId = do
  now <- getCurrentTime
  update' $ \tbl -> do
    set
      tbl
      [ RegistrationTokenVerified =. val True,
        RegistrationTokenUpdatedAt =. val now
      ]
    where_ $ tbl ^. RegistrationTokenTId ==. val (toKey rtId)

findByToken :: Transactionable m => RegToken -> m (Maybe RegistrationToken)
findByToken token =
  findOne $ do
    regToken <- from $ table @RegistrationTokenT
    where_ $ regToken ^. RegistrationTokenToken ==. val token
    return regToken

updateAttempts :: Int -> Id RegistrationToken -> SqlDB ()
updateAttempts attemps rtId = do
  now <- getCurrentTime
  update' $ \tbl -> do
    set
      tbl
      [ RegistrationTokenAttempts =. val attemps,
        RegistrationTokenUpdatedAt =. val now
      ]
    where_ $ tbl ^. RegistrationTokenTId ==. val (toKey rtId)

deleteByPersonId :: Id Person -> SqlDB ()
deleteByPersonId personId =
  Esq.delete' $ do
    regToken <- from $ table @RegistrationTokenT
    where_ $ regToken ^. RegistrationTokenEntityId ==. val (getId personId)

deleteByPersonIdExceptNew :: Id Person -> Id RegistrationToken -> SqlDB ()
deleteByPersonIdExceptNew personId newRT =
  Esq.delete' $ do
    regToken <- from $ table @RegistrationTokenT
    where_ $
      regToken ^. RegistrationTokenEntityId ==. val (getId personId)
        &&. not_ (regToken ^. RegistrationTokenTId ==. val (toKey newRT))

findAllByPersonId :: Transactionable m => Id Person -> m [RegistrationToken]
findAllByPersonId personId =
  findAll $ do
    regToken <- from $ table @RegistrationTokenT
    where_ $ regToken ^. RegistrationTokenEntityId ==. val (getId personId)
    return regToken
