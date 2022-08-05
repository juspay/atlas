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
import App.Scheduler
import Beckn.Prelude
import Beckn.Types.APISuccess (APISuccess (Success))
import Beckn.Utils.Common
import Environment
import Servant

handler :: FlowServer API.API
handler = helloHandler :<|> createJobHandler

helloHandler :: FlowHandler Text
helloHandler = withFlowHandlerAPI $ pure "Hello, world!"

createJobHandler :: Text -> FlowHandler APISuccess
createJobHandler jobType = withFlowHandlerAPI $ do
  case jobType of
    "bananas" -> void $ createBananasCountingJob 7
    "failing_time" -> void $ createTimePrinterJob 7
    "incorrect_data" -> void $ createIncorrectDataJob 7
    "fake_job" -> void $ createFakeJob 7
    "test_termination" -> void $ createTestTerminationJob 5
    _ -> logWarning "unknown job type"
  pure Success
