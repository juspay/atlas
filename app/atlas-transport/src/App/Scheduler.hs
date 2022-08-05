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

Module      :  App.Scheduler
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module App.Scheduler where

import App.Types (AppCfg, Log (withLogTag))
import Beckn.Mock.App (runMock)
import Beckn.Prelude
import Beckn.Scheduler
import Beckn.Storage.Esqueleto (HasEsqEnv)
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Storage.Esqueleto.Config (EsqDBEnv, prepareEsqDBEnv)
import Beckn.Types.Id (Id (Id), ShortId)
import Beckn.Utils.Common (HasPrettyLogger, LogLevel (DEBUG), MonadGuid (generateGUIDText), MonadTime (getCurrentTime), logInfo, logPretty)
import Beckn.Utils.Dhall
import Beckn.Utils.IOLogging (LoggerConfig, LoggerEnv, prepareLoggerEnv)
import qualified Control.Monad.Catch as C
import Data.String.Conversions (cs)
import Domain.Types.Organization (Organization)
import qualified Domain.Types.RideBooking as DRB
import qualified Domain.Types.RideRequest as RideRequest
import qualified Storage.Queries.RideRequest as RideRequest
import System.Environment (lookupEnv)

runTransporterScheduler ::
  (SchedulerConfig JobType -> SchedulerConfig JobType) ->
  (AppCfg -> AppCfg) ->
  IO ()
runTransporterScheduler configModifier transporterConfigModifier = do
  appCfg <- configModifier <$> readDhallConfigDefault "transporter-scheduler"
  hostname <- fmap cs <$> lookupEnv "POD_NAME" :: IO (Maybe Text)
  appCfgTransporter <- transporterConfigModifier <$> readDhallConfigDefault "atlas-transport"
  loggerEnv <- prepareLoggerEnv appCfg.loggerConfig hostname
  esqDBEnv <- prepareEsqDBEnv appCfgTransporter.esqDBCfg loggerEnv
  let loggerConfig = appCfg.loggerConfig
  let handlerEnv = HandlerEnv {..}
  runScheduler appCfg $ schedulerHandlerList handlerEnv

schedulerHandlerList :: HandlerEnv -> JobHandlerList JobType
schedulerHandlerList env =
  [ (AllocateRental, JobHandler $ \x -> runMock env $ allocateRentalRide x)
  ]

data JobType = AllocateRental | FakeType
  deriving (Generic, FromDhall, Eq, Ord, Show, FromJSON, ToJSON)

data HandlerEnv = HandlerEnv
  { loggerConfig :: LoggerConfig,
    loggerEnv :: LoggerEnv,
    esqDBEnv :: EsqDBEnv
  }

--------------------------------------

data AllocateRentalJobData = AllocateRentalJobData
  { rideBookingId :: Id DRB.RideBooking,
    shortOrgId :: ShortId Organization
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

allocateRentalRide ::
  (HasEsqEnv r m, Log m, HasPrettyLogger m r, C.MonadCatch m, MonadGuid m) =>
  Job JobType AllocateRentalJobData ->
  m ExecutionResult
allocateRentalRide job = C.handleAll (const $ pure Retry) $
  withLogTag ("JobId=" <> job.id.getId) $ do
    guid <- Id <$> generateGUIDText
    now <- getCurrentTime
    let rideReq =
          RideRequest.RideRequest
            { id = guid,
              createdAt = now,
              rideBookingId = job.jobData.rideBookingId,
              shortOrgId = job.jobData.shortOrgId,
              _type = RideRequest.ALLOCATION,
              info = Nothing
            }
    logInfo $ "allocating rental ride for rideReqestId=" <> job.jobData.rideBookingId.getId
    logPretty DEBUG "ride request" rideReq
    Esq.runTransaction $ RideRequest.create rideReq
    pure Complete
