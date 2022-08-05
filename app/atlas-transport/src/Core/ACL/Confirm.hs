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

import Beckn.Product.Validation.Context
import qualified Beckn.Types.Core.Context as Context
import qualified Beckn.Types.Core.Taxi.API.Confirm as Confirm
import Beckn.Types.Id
import Beckn.Types.Registry.Subscriber
import qualified Control.Monad.Catch as C
import Domain.Action.Beckn.Confirm
import EulerHS.Prelude hiding (id)
import Types.Error
import Utils.Common

buildConfirmReq ::
  (C.MonadCatch m, HasFlowEnv m r ["coreVersion" ::: Text, "domainVersion" ::: Text]) =>
  Subscriber ->
  Confirm.ConfirmReq ->
  m DConfirmReq
buildConfirmReq subscriber req = do
  let context = req.context
  validateContext Context.CONFIRM context
  let items = req.message.order.items
  item <- case items of
    [] -> throwError (InvalidRequest "List of confirmed items is empty.")
    [item] -> return item
    _ -> throwError (InvalidRequest "List of confirmed items must contain exactly one item.")
  unless (subscriber.subscriber_id == context.bap_id) $
    throwError (InvalidRequest "Invalid bap_id")
  unless (subscriber.subscriber_url == context.bap_uri) $
    throwError (InvalidRequest "Invalid bap_uri") -- is it correct?
  let phone = req.message.order.fulfillment.customer.contact.phone
  pure $
    DConfirmReq
      { quoteId = Id item.id,
        customerMobileCountryCode = phone.country_code,
        customerPhoneNumber = phone.number,
        bapUri = req.context.bap_uri,
        bapId = req.context.bap_id
      }
