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

Module      :  Storage.Queries.NotificationStatus
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Storage.Queries.NotificationStatus where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.NotificationStatus as NotificationStatus
import Domain.Types.RideBooking
import Storage.Tabular.NotificationStatus
import Types.App
import Utils.Common

createMany :: [NotificationStatus] -> SqlDB ()
createMany = createMany'

updateStatus :: Id RideBooking -> AnswerStatus -> [Id Driver] -> SqlDB ()
updateStatus rideBookingId status driverIds =
  update' $ \tbl -> do
    set
      tbl
      [ NotificationStatusStatus =. val status
      ]
    where_ $
      tbl ^. NotificationStatusRideBookingId ==. val (toKey rideBookingId)
        &&. tbl ^. NotificationStatusDriverId `in_` valList (toKey . cast <$> driverIds)

fetchAttemptedNotificationsByRBId :: Transactionable m => Id RideBooking -> m [NotificationStatus]
fetchAttemptedNotificationsByRBId rideBookingId =
  Esq.findAll $ do
    notificationStatus <- from $ table @NotificationStatusT
    where_ $
      notificationStatus ^. NotificationStatusRideBookingId ==. val (toKey rideBookingId)
        &&. notificationStatus ^. NotificationStatusStatus `in_` valList [NotificationStatus.REJECTED, NotificationStatus.IGNORED]
    return notificationStatus

fetchActiveNotifications :: Transactionable m => m [NotificationStatus]
fetchActiveNotifications =
  Esq.findAll $ do
    notificationStatus <- from $ table @NotificationStatusT
    where_ $
      notificationStatus ^. NotificationStatusStatus ==. val NotificationStatus.NOTIFIED
    return notificationStatus

findActiveNotificationByRBId :: Transactionable m => Id RideBooking -> m [NotificationStatus.NotificationStatus]
findActiveNotificationByRBId rideBookingId =
  Esq.findAll $ do
    notificationStatus <- from $ table @NotificationStatusT
    where_ $
      notificationStatus ^. NotificationStatusRideBookingId ==. val (toKey rideBookingId)
        &&. notificationStatus ^. NotificationStatusStatus ==. val NotificationStatus.NOTIFIED
    return notificationStatus

findActiveNotificationByDriverId :: Transactionable m => Id Driver -> Id RideBooking -> m (Maybe NotificationStatus)
findActiveNotificationByDriverId driverId rideBookingId =
  Esq.findOne $ do
    notificationStatus <- from $ table @NotificationStatusT
    where_ $
      notificationStatus ^. NotificationStatusRideBookingId ==. val (toKey rideBookingId)
        &&. notificationStatus ^. NotificationStatusStatus ==. val NotificationStatus.NOTIFIED
        &&. notificationStatus ^. NotificationStatusDriverId ==. val (toKey $ cast driverId)
    return notificationStatus

cleanupNotifications :: Id RideBooking -> SqlDB ()
cleanupNotifications rideBookingId =
  Esq.delete' $ do
    notificationStatus <- from $ table @NotificationStatusT
    where_ (notificationStatus ^. NotificationStatusRideBookingId ==. val (toKey rideBookingId))

cleanupOldNotifications :: SqlDB Int
cleanupOldNotifications = do
  compareTime <- getCurrentTime <&> addUTCTime (-300) -- We only remove very old notifications (older than 5 minutes) as a fail-safe
  res <- Esq.deleteReturningCount' $ do
    notificationStatus <- from $ table @NotificationStatusT
    where_ (notificationStatus ^. NotificationStatusExpiresAt ==. val compareTime)
  return $ fromIntegral res
