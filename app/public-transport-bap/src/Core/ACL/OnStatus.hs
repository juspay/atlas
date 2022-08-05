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

Module      :  Core.ACL.OnStatus
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Core.ACL.OnStatus where

import Beckn.Prelude
import Beckn.Types.Id
import Core.ACL.Common.MakeStatus
import qualified Core.Spec.Common.Payment as Payment
import qualified Core.Spec.OnStatus as OnStatus
import qualified Domain.Action.Beckn.OnStatus as DOnStatus
import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.PaymentTransaction as DPaymentTransaction

mkOnStatus :: OnStatus.OnStatusMessage -> Text -> DOnStatus.OnStatusReq
mkOnStatus msg txnId = do
  let payment = msg.order.payment
      bppOrderStatus = msg.order.state
      bppPaymentStatus = payment.status
      bppPaymentGatewayTxnStatus = payment.params.transaction_status
      paymentStatus = mkPaymentStatus (bppPaymentStatus, bppPaymentGatewayTxnStatus)
      bookingStatus = mkBookingStatus bppPaymentStatus bppOrderStatus
      domainReq = mkDomainOnStatusReq txnId bookingStatus bppPaymentGatewayTxnStatus paymentStatus
  domainReq

mkDomainOnStatusReq ::
  Text ->
  DBooking.BookingStatus ->
  Payment.TrStatus ->
  DPaymentTransaction.PaymentStatus ->
  DOnStatus.OnStatusReq
mkDomainOnStatusReq txnId bookingStatus transactionStatus paymentStatus =
  DOnStatus.OnStatusReq
    { bookingId = Id txnId,
      transactionStatus = show transactionStatus,
      ..
    }
