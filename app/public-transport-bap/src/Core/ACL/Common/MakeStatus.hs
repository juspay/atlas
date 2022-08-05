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

Module      :  Core.ACL.Common.MakeStatus
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Core.ACL.Common.MakeStatus where

import qualified Core.Spec.Common.OrderState as OrderState
import qualified Core.Spec.Common.Payment as Payment
import qualified Domain.Types.Booking as Domain
import qualified Domain.Types.PaymentTransaction as Domain

mkBookingStatus :: Payment.Status -> OrderState.State -> Domain.BookingStatus
mkBookingStatus st = \case
  OrderState.ACTIVE -> case st of
    Payment.NOT_PAID -> Domain.AWAITING_PAYMENT
    Payment.PAID -> Domain.CONFIRMED
  OrderState.COMPLETE -> Domain.CONFIRMED
  OrderState.CANCELLED -> Domain.CANCELLED

mkPaymentStatus ::
  (Payment.Status, Payment.TrStatus) ->
  Domain.PaymentStatus
mkPaymentStatus = \case
  (Payment.NOT_PAID, Payment.PAYMENT_LINK_CREATED) -> Domain.PENDING
  (Payment.NOT_PAID, Payment.PAYMENT_LINK_EXPIRED) -> Domain.FAILED
  (Payment.NOT_PAID, _) -> Domain.PENDING
  (Payment.PAID, Payment.CAPTURED) -> Domain.SUCCESS
  (Payment.PAID, Payment.REFUNDED) -> Domain.FAILED
  (Payment.PAID, _) -> Domain.PENDING
