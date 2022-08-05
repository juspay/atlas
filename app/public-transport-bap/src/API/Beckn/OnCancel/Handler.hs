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

Module      :  API.Beckn.OnCancel.Handler
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module API.Beckn.OnCancel.Handler where

import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as Esq
import qualified Beckn.Storage.Queries.BecknRequest as QBR
import Beckn.Types.Core.Ack
import Beckn.Types.Core.ReqTypes
import Beckn.Utils.Common
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import qualified Core.ACL.OnStatus as BecknACL
import Core.Context (validateContext)
import Core.Spec.API.OnCancel
import qualified Core.Spec.Common.Context as Context
import qualified Core.Spec.OnCancel as OnCancel
import Core.Spec.OnStatus
import Data.Aeson (encode)
import qualified Domain.Action.Beckn.OnStatus as DOnStatus
import Environment
import Tools.Error

handler ::
  SignatureAuthResult ->
  FlowServer OnCancelAPI
handler = onCancel

onCancel ::
  SignatureAuthResult ->
  BecknCallbackReq OnCancel.OnCancelMessage ->
  FlowHandler AckResponse
onCancel (SignatureAuthResult signPayload _) req = withFlowHandlerBecknAPI . withTransactionIdLogTag req $ do
  validateContext Context.ON_CANCEL $ req.context
  case req.contents of
    Right msg -> do
      transactionId <- req.context.transaction_id & fromMaybeM (InvalidRequest "Context.transaction_id is not present.")
      let order = msg.order
      let domainReq = BecknACL.mkOnStatus (OnStatusMessage order) transactionId
      logPretty DEBUG "domain request" domainReq
      DOnStatus.handler domainReq
      Esq.runTransaction $
        QBR.logBecknRequest (show $ encode req) (show $ signPayload.signature)
    Left err -> logTagError "on_cancel req" $ "on_cancel error: " <> show err
  pure Ack
