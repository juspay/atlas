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

Module      :  Storage.Queries.FarePolicy.PerExtraKmRate

Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Storage.Queries.FarePolicy.PerExtraKmRate
  ( Storage.Queries.FarePolicy.PerExtraKmRate.findAll,
    deleteAll,
  )
where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.FarePolicy.PerExtraKmRate
import Domain.Types.Organization (Organization)
import Storage.Tabular.FarePolicy.PerExtraKmRate
import Types.Error (FarePolicyError (NoPerExtraKmRate))
import Utils.Common

findAll ::
  ( Transactionable m,
    Monad m,
    MonadThrow m,
    Log m
  ) =>
  Id Organization ->
  m (NonEmpty PerExtraKmRate)
findAll orgId = do
  rez <- Esq.findAll $ do
    perExtraKmRate <- from $ table @PerExtraKmRateT
    where_ $
      perExtraKmRate ^. PerExtraKmRateOrganizationId ==. val (toKey orgId)
    orderBy [asc $ perExtraKmRate ^. PerExtraKmRateDistanceRangeStart]
    return perExtraKmRate
  noneEmptyRez <- case rez of
    e : es -> pure $ e :| es
    [] -> throwError NoPerExtraKmRate
  return (getDomainPart <$> noneEmptyRez)

deleteAll :: Id Organization -> SqlDB ()
deleteAll orgId =
  delete' $ do
    perExtraKmRate <- from $ table @PerExtraKmRateT
    where_ $
      perExtraKmRate ^. PerExtraKmRateOrganizationId ==. val (toKey orgId)

getDomainPart :: FullPerExtraKmRate -> PerExtraKmRate
getDomainPart (_, _, domain) = domain
