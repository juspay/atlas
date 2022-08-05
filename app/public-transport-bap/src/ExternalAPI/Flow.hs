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

Module      :  ExternalAPI.Flow
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module ExternalAPI.Flow where

import Beckn.Prelude
import Beckn.Tools.Metrics.CoreMetrics
import Beckn.Types.App
import Beckn.Types.Core.ReqTypes
import Beckn.Utils.Common
import Beckn.Utils.Error.BaseError.HTTPError.BecknAPIError (IsBecknAPI)
import Beckn.Utils.Servant.SignatureAuth
import qualified Core.Spec.API.Confirm as Confirm
import qualified Core.Spec.API.Status as Status
import Core.Spec.Confirm
import qualified Core.Spec.Status as Status
import GHC.Records.Extra

confirm ::
  ( MonadFlow m,
    MonadReader r m,
    CoreMetrics m,
    HasField "selfId" r Text
  ) =>
  BaseUrl ->
  BecknReq ConfirmMessage ->
  m ()
confirm bppUrl req = do
  callBecknAPIWithSignature "confirm" Confirm.confirmAPI bppUrl req

status ::
  ( MonadFlow m,
    MonadReader r m,
    CoreMetrics m,
    HasField "selfId" r Text
  ) =>
  BaseUrl ->
  BecknReq Status.StatusMessage ->
  m ()
status = callBecknAPIWithSignature "status" Status.statusAPI

callBecknAPIWithSignature ::
  ( MonadFlow m,
    MonadReader r m,
    CoreMetrics m,
    IsBecknAPI api req res,
    HasField "selfId" r Text
  ) =>
  Text ->
  Proxy api ->
  BaseUrl ->
  req ->
  m ()
callBecknAPIWithSignature a b c d = do
  bapId <- asks (.selfId)
  void $ callBecknAPI (Just $ getHttpManagerKey bapId) Nothing a b c d
