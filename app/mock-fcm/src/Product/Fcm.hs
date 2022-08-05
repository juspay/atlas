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

Module      :  Product.Fcm
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Product.Fcm where

import App.Types
import Beckn.External.FCM.Types
import Beckn.Types.Error
import Beckn.Utils.Error
import Beckn.Utils.Logging
import Beckn.Utils.Text
import Control.Concurrent.MVar (modifyMVar, modifyMVar_)
import qualified Data.Map as Map
import EulerHS.Prelude
import Types.API.Fcm

sendFcm ::
  Maybe FCMAuthToken ->
  FCMRequest ->
  FlowHandler FCMResponse
sendFcm _authToken (FCMRequest ntf) = withFlowHandler $ do
  to <- ntf.fcmToken & fromMaybeM (InvalidRequest "No token")
  log INFO $ "Message for " <> encodeToText to <> ": " <> encodeToText ntf
  asks notificationsMap >>= liftIO . (`modifyMVar_` (pure . set to))
  return $ FCMResponse Nothing Nothing
  where
    set to ntfs = Map.insert to (ntf : fromMaybe [] (Map.lookup to ntfs)) ntfs

readFcm :: FCMRecipientToken -> FlowHandler ReadFcmRes
readFcm number = withFlowHandler $ do
  asks notificationsMap >>= liftIO . (`modifyMVar` (pure . take'))
  where
    take' ntfs = Map.lookup number ntfs & maybe (ntfs, []) (Map.delete number ntfs,)
