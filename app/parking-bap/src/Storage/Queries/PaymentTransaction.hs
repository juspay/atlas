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

Module      :  Storage.Queries.PaymentTransaction
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Storage.Queries.PaymentTransaction where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Common
import Beckn.Types.Id
import Domain.Booking
import Domain.PaymentTransaction
import Storage.Tabular.PaymentTransaction

findById :: EsqDBFlow m r => Id PaymentTransaction -> m (Maybe PaymentTransaction)
findById = Esq.findById

findByBookingId :: EsqDBFlow m r => Id Booking -> m (Maybe PaymentTransaction)
findByBookingId bookingId =
  findOne $ do
    parkingSearch <- from $ table @PaymentTransactionT
    where_ $ parkingSearch ^. PaymentTransactionBookingId ==. val (toKey bookingId)
    return parkingSearch

create :: PaymentTransaction -> SqlDB ()
create = create'

updateTxnDetails :: Id PaymentTransaction -> Text -> PaymentStatus -> SqlDB ()
updateTxnDetails paymentTransactionId paymentGatewayTxnStatus newStatus = do
  now <- getCurrentTime
  update' $ \tbl -> do
    set
      tbl
      [ PaymentTransactionStatus =. val newStatus,
        PaymentTransactionPaymentGatewayTxnStatus =. val paymentGatewayTxnStatus,
        PaymentTransactionUpdatedAt =. val now
      ]
    where_ $ tbl ^. PaymentTransactionId ==. val (getId paymentTransactionId)
