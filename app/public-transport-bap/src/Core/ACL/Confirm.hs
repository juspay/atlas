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

Module      :  Core.ACL.Confirm
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Core.ACL.Confirm where

import Beckn.Prelude
import Core.Spec.Common
import Core.Spec.Confirm
import Domain.Action.UI.QuoteConfirm
import Servant.Client

mkConfirmMessage :: ConfirmMessageD -> Order
mkConfirmMessage msg = do
  let dQuote = msg.quote
  let provider = ProviderId "" -- ?
      item =
        Item
          { route_code = dQuote.routeCode,
            start_stop = dQuote.departureStationId.getId,
            end_stop = dQuote.arrivalStationId.getId,
            start_time = dQuote.departureTime,
            end_time = dQuote.arrivalTime
          }
      quantity = msg.quantity
      items = replicate quantity item
      billing =
        Billing
          { name = msg.requestorName
          }
      onePrice = rupeePrice dQuote.fare
      totalAmount = dQuote.fare * fromIntegral quantity
      totalPrice = rupeePrice totalAmount
      breakupTitle = dQuote.description
      breakupItem = BreakupItem breakupTitle onePrice
      quote =
        Quotation
          { price = totalPrice,
            breakup = replicate quantity breakupItem,
            ttl = Nothing
          }
      payment =
        Payment
          { uri = BaseUrl Http "fake.org" 80 "fake",
            tl_method = HttpGet,
            params = rupeeParams totalAmount,
            _type = PRE_FULFILLMENT,
            status = NOT_PAID
          }
  Order {..}
