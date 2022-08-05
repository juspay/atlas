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

Module      :  API.UI.QuoteConfirm.Handler
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module API.UI.QuoteConfirm.Handler where

import Beckn.Prelude
import Beckn.Types.Core.ReqTypes (BecknReq (BecknReq))
import Beckn.Types.Id
import Beckn.Utils.Common
import Core.ACL.Confirm
import Core.Context
import Core.Spec.Common.Context
import Core.Spec.Confirm (ConfirmMessage (ConfirmMessage))
import Domain.Action.UI.QuoteConfirm
import qualified Domain.Types.Quote as D
import Environment
import qualified ExternalAPI.Flow as ExternalAPI
import Tools.Auth (PersonId)

handler :: PersonId -> Id D.Quote -> QConfirmReq -> FlowHandler QConfirmRes
handler personId quoteId confirmReq = withFlowHandlerAPI $ do
  (res, becknReq) <- quoteConfirm personId quoteId confirmReq
  callConfirm becknReq
  pure res

callConfirm :: ConfirmMessageD -> Flow ()
callConfirm msg = do
  selfUrl <- asks (.selfURI)
  selfId <- asks (.selfId)
  context <- buildContext CONFIRM msg.txnId selfId selfUrl Nothing Nothing
  let confirmOrder = mkConfirmMessage msg
  ExternalAPI.confirm msg.booking.bppUrl (BecknReq context $ ConfirmMessage confirmOrder)
