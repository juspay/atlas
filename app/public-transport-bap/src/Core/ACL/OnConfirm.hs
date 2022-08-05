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

Module      :  Core.ACL.OnConfirm
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Core.ACL.OnConfirm where

import Beckn.Prelude
import Beckn.Types.Id
import Core.ACL.Common.MakeStatus (mkBookingStatus, mkPaymentStatus)
import Core.Spec.OnConfirm
import Domain.Action.Beckn.OnConfirm
import qualified Domain.Types.Booking as Domain

mkDomainOnConfirm :: Id Domain.Booking -> OnConfirmMessage -> OnConfirmMessageD
mkDomainOnConfirm bookingId msg = do
  -- it may not be so obvious why booking id is the same as transaction id
  let ticketId = msg.order.id
      paymentGatewayTxnId = msg.order.payment.params.transaction_id
      paymentGatewayTxnStatus = show msg.order.payment.params.transaction_status
      paymentUrl = msg.order.payment.uri
      bppOrderStatus = msg.order.state
      payment = msg.order.payment
      bppPaymentStatus = payment.status
      bppPaymentGatewayTxnStatus = payment.params.transaction_status
      paymentStatus = mkPaymentStatus (bppPaymentStatus, bppPaymentGatewayTxnStatus)
      bookingStatus = mkBookingStatus bppPaymentStatus bppOrderStatus

  OnConfirmMessageD {..}
