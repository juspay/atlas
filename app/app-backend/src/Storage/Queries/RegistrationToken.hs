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
import Beckn.Types.Common
import Beckn.Types.Id
import Domain.Types.Person
import Domain.Types.RegistrationToken
import Storage.Tabular.RegistrationToken

create :: RegistrationToken -> SqlDB ()
create = create'

findById :: Transactionable m => Id RegistrationToken -> m (Maybe RegistrationToken)
findById = Esq.findById

findByToken :: Transactionable m => Text -> m (Maybe RegistrationToken)
findByToken token_ =
  findOne $ do
    registrationToken <- from $ table @RegistrationTokenT
    where_ $ registrationToken ^. RegistrationTokenToken ==. val token_
    return registrationToken

setVerified :: Id RegistrationToken -> SqlDB ()
setVerified rtId = do
  now <- getCurrentTime
  update' $ \tbl -> do
    set
      tbl
      [ RegistrationTokenUpdatedAt =. val now,
        RegistrationTokenVerified =. val True
      ]
    where_ $ tbl ^. RegistrationTokenId ==. val (getId rtId)

updateAttempts :: Int -> Id RegistrationToken -> SqlDB ()
updateAttempts attemps rtId = do
  now <- getCurrentTime
  update' $ \tbl -> do
    set
      tbl
      [ RegistrationTokenUpdatedAt =. val now,
        RegistrationTokenAttempts =. val attemps
      ]
    where_ $ tbl ^. RegistrationTokenId ==. val (getId rtId)

deleteByPersonId :: Id Person -> SqlDB ()
deleteByPersonId (Id personId) = do
  delete' $ do
    registrationToken <- from $ table @RegistrationTokenT
    where_ (registrationToken ^. RegistrationTokenEntityId ==. val personId)

deleteByPersonIdExceptNew :: Id Person -> Id RegistrationToken -> SqlDB ()
deleteByPersonIdExceptNew (Id personId) newRT = do
  delete' $ do
    registrationToken <- from $ table @RegistrationTokenT
    where_ $
      (registrationToken ^. RegistrationTokenEntityId ==. val personId)
        &&. not_ (registrationToken ^. RegistrationTokenId ==. val (getId newRT))

findAllByPersonId :: Transactionable m => Id Person -> m [RegistrationToken]
findAllByPersonId (Id personId) =
  findAll $ do
    registrationToken <- from $ table @RegistrationTokenT
    where_ $ registrationToken ^. RegistrationTokenEntityId ==. val personId
    return registrationToken
