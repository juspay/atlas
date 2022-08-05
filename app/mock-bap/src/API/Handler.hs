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

Module      :  API.Handler
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module API.Handler where

import qualified API.Types as API
import Beckn.Prelude
import Beckn.Tools.Metrics.CoreMetrics
import Beckn.Types.App
import Beckn.Types.Core.Ack
import Beckn.Utils.Common
import qualified Beckn.Utils.Error.BaseError.HTTPError.BecknAPIError as Beckn
import Beckn.Utils.Servant.JSONBS
import Beckn.Utils.Servant.SignatureAuth
import qualified Data.ByteString as BS
import Environment
import Servant

handler :: FlowServer API.API
handler = trigger :<|> callbackReceiver

trigger :: Text -> BS.ByteString -> FlowHandler AckResponse
trigger urlText body = withFlowHandlerBecknAPI $ do
  url <- parseBaseUrl urlText
  logInfo $ decodeUtf8 body
  callBAP url body

callBAP ::
  ( MonadFlow m,
    HasFlowEnv m r '["selfId" ::: Text],
    CoreMetrics m
  ) =>
  BaseUrl ->
  BS.ByteString ->
  m AckResponse
callBAP uri body = do
  selfId <- asks (.selfId)
  let authKey = getHttpManagerKey selfId
  Beckn.callBecknAPI (Just authKey) Nothing "Some action" fakeAPI uri body
  where
    fakeAPI :: Proxy (ReqBody '[JSONBS] BS.ByteString :> Post '[JSON] AckResponse)
    fakeAPI = Proxy

callbackReceiver :: SignatureAuthResult -> Text -> BS.ByteString -> FlowHandler AckResponse
callbackReceiver _ action body = withFlowHandlerBecknAPI $ do
  logInfo $ "Received " <> action <> " callback with body: " <> decodeUtf8 body
  return Ack
