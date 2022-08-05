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

Module      :  Utils
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Utils where

import qualified "app-backend" App.Types as BecknApp
import qualified "atlas-transport" App.Types as BecknTransport
import Beckn.Types.Flow
import Beckn.Utils.Common
import Data.String.Conversions
import EulerHS.Prelude
import qualified EulerHS.Runtime as R
import GHC.IO (unsafePerformIO)
import HSpec
import Network.HTTP.Client (Manager)
import qualified Network.HTTP.Client as Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Resources (appBackendEnv, transporterAppEnv)
import Servant.Client hiding (client)

defaultTestLoggerConfig :: LoggerConfig
defaultTestLoggerConfig =
  LoggerConfig
    { level = DEBUG,
      logToFile = True,
      logFilePath = "/tmp/atlas-integ-test.log",
      logToConsole = False,
      logRawSql = True,
      prettyPrinting = True
    }

runClient :: (HasCallStack, MonadIO m) => ClientEnv -> ClientM a -> m (Either ClientError a)
runClient clientEnv x = liftIO $ runClientM x clientEnv

runClient' :: (HasCallStack, MonadIO m, Show a) => ClientEnv -> ClientM a -> m a
runClient' clientEnv x = do
  res <- runClient clientEnv x
  res `shouldSatisfy` isRight
  let Right r = res
  return r

-- | Invoke an action until getting 'Just'.
--
-- The second argument describes attempted delays prior to running an action,
-- in mcs.
pollWithDescription :: (HasCallStack, MonadIO m, MonadCatch m) => Text -> [Int] -> m (Maybe a) -> m a
pollWithDescription description allDelays action = withFrozenCallStack $ go allDelays
  where
    go [] =
      expectationFailure $
        "poll: failed to get an expected entry after "
          <> show (fromIntegral (sum allDelays) / 1e6 :: Float)
          <> " seconds;\ndescription: "
          <> cs description
    go (delay : remDelays) = do
      let printLastError err = do
            when (null remDelays) $ print ("Last error: " <> show err :: Text)
            return Nothing
      liftIO $ threadDelay delay
      try @_ @SomeException action >>= either printLastError return >>= maybe (go remDelays) pure

expBackoff :: Int -> Int -> [Int]
expBackoff startDelay maxDelay =
  0 : takeWhile (< maxDelay) (iterate (* 2) startDelay)

-- | 'pollWith' with default timing.
--
-- Optimized for requesting a server for a result of async action.
poll :: (HasCallStack, MonadIO m, MonadCatch m) => m (Maybe a) -> m a
poll = pollDesc ""

pollDesc :: (HasCallStack, MonadIO m, MonadCatch m) => Text -> m (Maybe a) -> m a
pollDesc description = pollWithDescription description (expBackoff 0.1e6 10e6)

runFlow :: (MonadIO m, Log (FlowR env)) => Text -> env -> FlowR env a -> m a
runFlow tag appEnv flow = do
  liftIO $
    R.withFlowRuntime Nothing $ \flowRt -> do
      runFlowR flowRt appEnv $ withLogTag tag flow

expectSingletonNE :: (HasCallStack, MonadIO m) => NonEmpty a -> m a
expectSingletonNE = \case
  a :| [] -> pure a
  l -> expectationFailure $ "Expected list with one element, got " <> show (length l) <> "elements"

expectSingletonList :: (HasCallStack, MonadIO m) => [a] -> m a
expectSingletonList = \case
  [a] -> pure a
  l -> expectationFailure $ "Expected list with one element, got " <> show (length l) <> "elements"

data ClientEnvs = ClientEnvs
  { bap :: ClientEnv,
    bpp :: ClientEnv
  }

type ClientsM = ReaderT ClientEnvs IO

withBecknClients :: ClientEnvs -> ClientsM a -> IO a
withBecknClients = flip runReaderT

callBAP, callBPP :: (HasCallStack, Show a) => ClientM a -> ClientsM a
callBAP client = asks (.bap) >>= (`runClient'` client)
callBPP client = asks (.bpp) >>= (`runClient'` client)

mkMobilityClients :: BaseUrl -> BaseUrl -> IO ClientEnvs
mkMobilityClients bapUrl bppUrl = do
  pure $
    ClientEnvs
      { bap = mkClientEnv defaultManager bapUrl,
        bpp = mkClientEnv defaultManager bppUrl
      }

{-# NOINLINE defaultManager #-}
defaultManager :: Manager
defaultManager = unsafePerformIO $ Client.newManager tlsManagerSettings

runAppFlow :: Text -> FlowR BecknApp.AppEnv a -> IO a
runAppFlow tag = runFlow tag appBackendEnv

runTransporterFlow :: Text -> FlowR BecknTransport.AppEnv a -> IO a
runTransporterFlow tag = runFlow tag transporterAppEnv
