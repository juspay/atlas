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

Module      :  API.Beckn.OnConfirm.Handler
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module API.Beckn.OnConfirm.Handler where

import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as Esq
import qualified Beckn.Storage.Queries.BecknRequest as QBR
import Beckn.Types.Id
import Beckn.Utils.Common
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import Core.ACL.OnConfirm
import Core.Context
import Core.Spec.API.OnConfirm
import qualified Core.Spec.Common.Context as Context
import Data.Aeson (encode)
import Domain.Action.Beckn.OnConfirm
import Environment
import Tools.Error

handler :: SignatureAuthResult -> FlowServer OnConfirmAPI
handler (SignatureAuthResult signPayload _) onConfirmCb = withFlowHandlerAPI . withTransactionIdLogTag onConfirmCb $ do
  validateContext Context.ON_CONFIRM $ onConfirmCb.context
  transactionId <- onConfirmCb.context.transaction_id & fromMaybeM (InvalidRequest "Context.transaction_id is not present.")
  case onConfirmCb.contents of
    Right msg -> do
      handleOnConfirm $ mkDomainOnConfirm (Id transactionId) msg
      Esq.runTransaction $
        QBR.logBecknRequest (show $ encode onConfirmCb) (show $ signPayload.signature)
    Left err -> logTagError "on_confirm req" $ "on_confirm error: " <> show err
  pure Ack
