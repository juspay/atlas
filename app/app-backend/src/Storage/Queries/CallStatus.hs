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

Module      :  Storage.Queries.CallStatus
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Storage.Queries.CallStatus where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.CallStatus
import Storage.Tabular.CallStatus
import qualified Types.API.Call as CallAPI

create :: CallStatus -> SqlDB ()
create = create'

findById :: Transactionable m => Id CallStatus -> m (Maybe CallStatus)
findById = Esq.findById

updateCallStatus :: Id CallStatus -> CallAPI.CallCallbackReq -> SqlDB ()
updateCallStatus callId req = do
  update' $ \tbl -> do
    set
      tbl
      [ CallStatusStatus =. val req.status,
        CallStatusConversationDuration =. val req.conversationDuration,
        CallStatusRecordingUrl =. val (Just (showBaseUrl req.recordingUrl))
      ]
    where_ $ tbl ^. CallStatusId ==. val (getId callId)
