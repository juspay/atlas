{-# OPTIONS_GHC -Wno-unused-do-bind #-}


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

Module      :  Services.DriverTrackingHealthcheck
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Services.DriverTrackingHealthcheck where

import App.DriverTrackingHealthcheck.Environment (AppEnv)
import Beckn.External.Encryption (decrypt)
import Beckn.External.FCM.Types (FCMNotificationType (TRIGGER_SERVICE))
import qualified Beckn.External.FCM.Types as FCM
import qualified Beckn.External.MyValueFirst.Flow as SF
import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Storage.Redis.Queries (lpush, rpop, tryLockRedis, unlockRedis)
import Beckn.Types.Common
import Beckn.Types.Error (PersonError (PersonFieldNotPresent))
import Beckn.Types.Flow (FlowR)
import Beckn.Types.Id (Id, cast)
import Data.Either
import Data.List.NonEmpty (nonEmpty)
import qualified Domain.Types.Person as SP
import qualified Product.HealthCheck as HC
import qualified Storage.Queries.DriverInformation as DrInfo
import Types.App (Driver)
import Utils.Common
import Utils.Notifications (notifyDevice)

type Flow = FlowR AppEnv

-- TODO: move this function somewhere
withLock :: Flow () -> Flow ()
withLock f = do
  getLock
  f `catch` (log ERROR . makeLogSomeException)
  unlockRedis lockKey
  where
    getLock = do
      lockAvailable <- tryLockRedis lockKey 10
      unless lockAvailable getLock
    lockKey = "atlas:" <> serviceName <> ":lock"

driverTrackingHealthcheckService :: Flow ()
driverTrackingHealthcheckService = withLogTag "driverTrackingHealthcheckService" do
  driverLastLocationUpdateCheckService
  driverDevicePingService
  driverMakingInactiveService

driverLastLocationUpdateCheckService :: Flow ()
driverLastLocationUpdateCheckService = service "driverLastLocationUpdateCheckService" $ withRandomId do
  delay <- asks (.driverAllowedDelay)
  withLock $ measuringDurationToLog INFO "driverLastLocationUpdateCheckService" do
    now <- getCurrentTime
    HC.iAmAlive serviceName
    drivers <- DrInfo.getDriversWithOutdatedLocationsToMakeInactive (negate (fromIntegral delay) `addUTCTime` now)
    let driverDetails = map fetchPersonIdAndMobileNumber drivers
    flip map driverDetails \case
      (driverId, Nothing) -> Left driverId
      (driverId, Just token) -> Right (driverId, token)
      & partitionEithers
      & \(noTokenIds, driversToPing) -> do
        unless (null noTokenIds) $ logPretty ERROR "Active drivers with no token" noTokenIds
        case nonEmpty driversToPing of
          Just driversWithToken -> do
            lpush redisKey driversWithToken
            logPretty INFO ("Drivers to ping: " <> show (length driversWithToken)) driversWithToken
          Nothing -> log INFO "No drivers to ping"
  threadDelay (secondsToMcs delay).getMicroseconds
  where
    fetchPersonIdAndMobileNumber :: SP.Person -> (Id Driver, Maybe FCM.FCMRecipientToken)
    fetchPersonIdAndMobileNumber driver = (cast driver.id :: Id Driver, driver.deviceToken)

serviceName :: Text
serviceName = "driver-tracking-healthcheck"

redisKey :: Text
redisKey = "atlas:driver-tracking-healthcheck:drivers-to-ping"

driverDevicePingService :: Flow ()
driverDevicePingService = service "driverDevicePingService" do
  HC.iAmAlive serviceName
  rpop redisKey >>= flip whenJust \(driverId, token) ->
    withLogTag driverId.getId do
      log INFO "Ping driver"
      notifyDevice TRIGGER_SERVICE "You were inactive" "Please check the app" driverId (Just token)
  asks (.notificationMinDelay)
    >>= threadDelay . (.getMicroseconds)

driverMakingInactiveService :: Flow ()
driverMakingInactiveService = service "driverMakingInactiveService" $ withRandomId do
  delay <- asks (.driverInactiveDelay)
  withLock $ measuringDurationToLog INFO "driverMakingInactiveService" do
    now <- getCurrentTime
    HC.iAmAlive serviceName
    drivers <- DrInfo.getDriversWithOutdatedLocationsToMakeInactive (negate (fromIntegral delay) `addUTCTime` now)
    logPretty INFO ("Drivers to make inactive: " <> show (length drivers)) ((.id) <$> drivers)
    mapM_ fetchPersonIdAndMobileNumber drivers
  threadDelay (secondsToMcs delay).getMicroseconds
  where
    fetchPersonIdAndMobileNumber :: SP.Person -> Flow ()
    fetchPersonIdAndMobileNumber driver = withLogTag ("driverId_" <> driver.id.getId) do
      mobileNumber' <- mapM decrypt driver.mobileNumber >>= fromMaybeM (PersonFieldNotPresent "mobileNumber")
      countryCode <- driver.mobileCountryCode & fromMaybeM (PersonFieldNotPresent "mobileCountryCode")
      log INFO "Make driver inactive"
      Esq.runTransaction $
        DrInfo.updateActivity (cast driver.id) False

      smsCfg <- asks (.smsCfg)
      driverInactiveSmsTemplate <- asks (.driverInactiveSmsTemplate)

      SF.sendSms smsCfg driverInactiveSmsTemplate (countryCode <> mobileNumber')
        >>= SF.checkSmsResult
