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

Module      :  Storage.Queries.FarePolicy.Discount
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Storage.Queries.FarePolicy.Discount where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.FarePolicy.Discount
import Domain.Types.Organization (Organization)
import Domain.Types.Vehicle as Vehicle
import Storage.Tabular.FarePolicy.Discount
import Utils.Common

create :: Discount -> SqlDB ()
create = create'

findById ::
  Transactionable m =>
  Id Discount ->
  m (Maybe Discount)
findById = Esq.findById

findAll ::
  Transactionable m =>
  Id Organization ->
  Vehicle.Variant ->
  m [Discount]
findAll orgId vehicleVariant =
  Esq.findAll $ do
    discount <- from $ table @DiscountT
    where_ $
      discount ^. DiscountOrganizationId ==. val (toKey orgId)
        &&. discount ^. DiscountVehicleVariant ==. val vehicleVariant
    return discount

update :: Id Discount -> Discount -> SqlDB ()
update discId disc = do
  now <- getCurrentTime
  update' $ \tbl -> do
    set
      tbl
      [ DiscountFromDate =. val disc.fromDate,
        DiscountToDate =. val disc.toDate,
        DiscountEnabled =. val disc.enabled,
        DiscountDiscount =. val (fromRational disc.discount),
        DiscountUpdatedAt =. val now
      ]
    where_ $ tbl ^. DiscountId ==. val (getId discId)

deleteById :: Id Discount -> SqlDB ()
deleteById = deleteByKey' @DiscountT
