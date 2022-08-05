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

Module      :  Product.Sms
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Product.Sms where

import App.Types
import Beckn.External.MyValueFirst.Types
import Beckn.Utils.Error.FlowHandling (withFlowHandler)
import Control.Concurrent.MVar (modifyMVar, modifyMVar_)
import qualified Data.Map as Map
import EulerHS.Prelude
import Types.API.Sms

sendSms :: Text -> Text -> Text -> Text -> Text -> FlowHandler SubmitSmsRes
sendSms _username _password _from to text = withFlowHandler $ do
  asks smsMap >>= liftIO . (`modifyMVar_` (pure . set))
  return Sent
  where
    set smss = Map.insert to (text : fromMaybe [] (Map.lookup to smss)) smss

readSms :: Text -> FlowHandler ReadSmsRes
readSms number = withFlowHandler $ do
  asks smsMap >>= liftIO . (`modifyMVar` (pure . take'))
  where
    take' smss = Map.lookup number smss & maybe (smss, []) (Map.delete number smss,)
