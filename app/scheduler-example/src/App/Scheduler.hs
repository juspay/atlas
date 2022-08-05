{-# LANGUAGE DerivingVia #-}


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

-- FIXME: This entire module is just for example
-- TODO: move it to the integration tests when real usage of the scheduler library appears.

import Beckn.Mock.App (MockM, runMock)
import Beckn.Prelude
import Beckn.Scheduler
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Error (GenericError (InternalError))
import Beckn.Types.Id
import Beckn.Utils.Common
import Beckn.Utils.Dhall (FromDhall, readDhallConfigDefault)
import Beckn.Utils.GenericPretty (PrettyShow, Showable (..))
import Beckn.Utils.IOLogging (LoggerEnv, prepareLoggerEnv)
import qualified Control.Monad.Catch as C
import Environment (Flow)
import System.Random

runExampleScheduler :: (SchedulerConfig JobType -> SchedulerConfig JobType) -> IO ()
runExampleScheduler configModifier = do
  appCfg <- configModifier <$> readDhallConfigDefault "scheduler-example-scheduler"
  let loggerConfig = appCfg.loggerConfig
  loggerEnv <- prepareLoggerEnv loggerConfig Nothing
  let loggerRes = LoggerResources {..}
  runScheduler appCfg $ schedulerHandlerList loggerRes

schedulerHandlerList :: LoggerResources -> JobHandlerList JobType
schedulerHandlerList loggerRes =
  [ (PrintBananasCount, JobHandler $ runMock loggerRes . bananasCounterHandler),
    (PrintCurrentTimeWithErrorProbability, JobHandler $ runMock loggerRes . timePrinterHandler),
    (IncorrectDataJobType, JobHandler $ runMock loggerRes . incorrectDataJobHandler),
    (TestTermination, JobHandler $ runMock loggerRes . testTerminationHandler)
  ]

-----------------

data LoggerResources = LoggerResources
  { loggerEnv :: LoggerEnv,
    loggerConfig :: LoggerConfig
  }

type SchedulerT = MockM LoggerResources

makeTestJobEntry :: JobType -> d -> JobEntry JobType d
makeTestJobEntry jType jData =
  JobEntry
    { jobType = jType,
      jobData = jData,
      maxErrors = 5
    }

data JobType
  = PrintBananasCount
  | PrintCurrentTimeWithErrorProbability
  | IncorrectDataJobType
  | FakeJobType
  | TestTermination
  deriving stock (Generic, Show, Eq, Ord)
  deriving anyclass (FromJSON, ToJSON, FromDhall)
  deriving (PrettyShow) via Showable JobType

-----------------
data BananasCount = BananasCount
  { count :: Int,
    createdAt :: UTCTime
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON, PrettyShow)

createBananasCountingJob :: NominalDiffTime -> Flow (Id (Job JobType BananasCount))
createBananasCountingJob scheduleIn = do
  now <- getCurrentTime
  bCount <- liftIO $ randomRIO (1, 10 :: Int)
  Esq.runTransaction $
    createJobIn scheduleIn $ makeTestJobEntry PrintBananasCount $ makeJobData now bCount
  where
    makeJobData now_ bCount_ =
      BananasCount
        { createdAt = now_,
          count = bCount_
        }

bananasCounterHandler :: Job JobType BananasCount -> SchedulerT ExecutionResult
bananasCounterHandler job = do
  logInfo "job of type 1 is being executed: printing job data"
  logPretty INFO "job data" job.jobData
  pure Complete

-----------------
createTimePrinterJob :: NominalDiffTime -> Flow (Id (Job JobType ()))
createTimePrinterJob scheduleIn =
  Esq.runTransaction $
    createJobIn scheduleIn $ makeTestJobEntry PrintCurrentTimeWithErrorProbability ()

timePrinterHandler :: Job JobType () -> SchedulerT ExecutionResult
timePrinterHandler _ = do
  logInfo "job of type 2 is being executed: trying to print current time with some probability of an error"
  randomNum <- liftIO $ randomRIO (1 :: Int, 6)
  if randomNum >= 3
    then throwError $ InternalError "Time printing error"
    else do
      now <- getCurrentTime
      logInfo $ "current time: " <> show now
      pure Complete

-----------------
createFakeJob :: NominalDiffTime -> Flow (Id (Job JobType ()))
createFakeJob scheduleIn =
  Esq.runTransaction $
    createJobIn scheduleIn $ makeTestJobEntry FakeJobType ()

-----------------
data IncorrectlySerializable = IncSer
  { foo :: Int,
    bar :: Text
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, PrettyShow)
  deriving (FromJSON) via JSONfail IncorrectlySerializable

newtype JSONfail a = JSONfail a

instance FromJSON (JSONfail a) where
  parseJSON _ = fail "fake fail"

createIncorrectDataJob :: NominalDiffTime -> Flow (Id (Job JobType IncorrectlySerializable))
createIncorrectDataJob scheduleIn =
  Esq.runTransaction $
    createJobIn scheduleIn $ makeTestJobEntry IncorrectDataJobType val
  where
    val = IncSer 2 "quux"

incorrectDataJobHandler :: Job JobType IncorrectlySerializable -> SchedulerT ExecutionResult
incorrectDataJobHandler _ = do
  logError "you shouldn't get here"
  pure Complete

-----------------
createTestTerminationJob :: NominalDiffTime -> Flow (Id (Job JobType ()))
createTestTerminationJob scheduleIn =
  Esq.runTransaction $
    createJobIn scheduleIn $ makeTestJobEntry TestTermination ()

testTerminationHandler :: Job JobType () -> SchedulerT ExecutionResult
testTerminationHandler _ = flip C.catchAll (\_ -> pure Retry) $ do
  logDebug "before pause"
  threadDelaySec 10
  logDebug "after pause"
  pure Complete
