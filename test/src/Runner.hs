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

Module      :  Runner

Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Runner
  ( withAppSettings,
    withApp,
  )
where

import Control.Concurrent.Async
import Control.Exception (ErrorCall (..), throwIO)
import EulerHS.Prelude
import Network.Wai
import Network.Wai.Handler.Warp

-- This module mirrors functions from warp, allowing to easily
-- spin up a server for testing purposes. The difference is that
-- while warp runs the server on a free port, withAppSettings and
-- withApp allow to specify the port to run the server on.

data Waiter a = Waiter
  { notify :: a -> IO (),
    waitFor :: IO a
  }

mkWaiter :: IO (Waiter a)
mkWaiter = do
  mvar <- newEmptyMVar
  return
    Waiter
      { notify = putMVar mvar,
        waitFor = readMVar mvar
      }

withAppSettings :: Settings -> Port -> IO Application -> IO a -> IO a
withAppSettings settings port mkApp action = do
  app <- mkApp
  started <- mkWaiter
  let appSettings =
        settings
          & setPort port
          & setBeforeMainLoop (notify started ())
  result <-
    race
      (runSettings appSettings app)
      (waitFor started >> action)
  case result of
    Left () -> throwIO $ ErrorCall "Unexpected: runSettings exited"
    Right x -> return x

withApp :: Port -> IO Application -> IO a -> IO a
withApp = withAppSettings defaultSettings
