{-# LANGUAGE TypeApplications #-}


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

Module      :  Storage.Queries.FarePolicy.FareProduct
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Storage.Queries.FarePolicy.FareProduct where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Common (MonadGuid (generateGUIDText), MonadTime (getCurrentTime))
import Beckn.Types.Id
import Domain.Types.FareProduct
import Domain.Types.Organization (Organization)
import Storage.Tabular.FareProduct

findEnabledByOrgId ::
  Transactionable m =>
  Id Organization ->
  m [FareProduct]
findEnabledByOrgId = findEnabledByOrgIdAndType Nothing

findEnabledByOrgIdAndType ::
  Transactionable m =>
  Maybe FareProductType ->
  Id Organization ->
  m [FareProduct]
findEnabledByOrgIdAndType mbType orgId =
  Esq.findAll $ do
    fareProduct <- from $ table @FareProductT
    where_ $
      fareProduct ^. FareProductOrganizationId ==. val (toKey orgId)
        &&. whenJust_ mbType (\typ -> fareProduct ^. FareProductProductType ==. val typ)
    pure fareProduct

upsertFareProduct ::
  Id Organization ->
  FareProductType ->
  SqlDB ()
upsertFareProduct orgId typ = do
  mbFp <- find predicate <$> findEnabledByOrgIdAndType (Just typ) orgId
  case mbFp of
    Nothing -> insertFareProduct
    Just _ -> pure ()
  where
    predicate :: FareProduct -> Bool
    predicate fp = fp._type == typ && fp.organizationId == orgId

    insertFareProduct :: SqlDB ()
    insertFareProduct = do
      now <- getCurrentTime
      guid <- Id <$> generateGUIDText
      Esq.create' $
        FareProduct
          { id = guid,
            organizationId = orgId,
            _type = typ,
            createdAt = now
          }

deleteFareProduct ::
  Id Organization ->
  FareProductType ->
  SqlDB ()
deleteFareProduct orgId fpType = Esq.delete' $ do
  fareProduct <- from $ table @FareProductT
  where_ $
    fareProduct ^. FareProductOrganizationId ==. val (toKey orgId)
      &&. fareProduct ^. FareProductProductType ==. val fpType
