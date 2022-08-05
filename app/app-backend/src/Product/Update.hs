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

Module      :  Product.Update
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Product.Update (onUpdate) where

import App.Types
import Beckn.Product.Validation.Context (validateContext)
import qualified Beckn.Storage.Esqueleto as DB
import qualified Beckn.Storage.Queries.BecknRequest as QBR
import Beckn.Types.Core.Ack
import qualified Beckn.Types.Core.Context as Context
import qualified Beckn.Types.Core.Taxi.API.OnUpdate as OnUpdate
import qualified Beckn.Types.Core.Taxi.OnUpdate as OnUpdate
import Beckn.Types.Id
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import Data.Aeson (encode)
import qualified Domain.Types.Ride as SRide
import qualified Domain.Types.RideBooking as SRB
import qualified Domain.Types.RideBookingCancellationReason as SBCR
import EulerHS.Prelude hiding (state)
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RideBooking as QRB
import qualified Storage.Queries.RideBookingCancellationReason as QBCR
import Tools.Metrics (CoreMetrics)
import Types.Error
import Utils.Common
import qualified Utils.Notifications as Notify

onUpdate ::
  SignatureAuthResult ->
  OnUpdate.OnUpdateReq ->
  FlowHandler AckResponse
onUpdate (SignatureAuthResult signPayload _) req = withFlowHandlerBecknAPI $
  withTransactionIdLogTag req $ do
    -- TODO: Verify api key here
    logTagInfo "on_update req" (show req)
    validateContext Context.ON_UPDATE req.context
    DB.runTransaction $
      QBR.logBecknRequest (show $ encode req) (show $ signPayload.signature)

    processEvent req.message.cabs_update_event
    return Ack

processEvent :: (EsqDBFlow m r, FCMFlow m r, CoreMetrics m) => OnUpdate.OnUpdateEvent -> m ()
processEvent (OnUpdate.RideAssigned taEvent) = do
  let bppBookingId = Id taEvent.order_id
  rideBooking <- QRB.findByBPPBookingId bppBookingId >>= fromMaybeM (RideBookingDoesNotExist bppBookingId.getId)
  unless (isAssignable rideBooking) $ throwError (RideBookingInvalidStatus $ show rideBooking.status)
  ride <- buildRide rideBooking
  DB.runTransaction $ do
    QRB.updateStatus rideBooking.id SRB.TRIP_ASSIGNED
    QRide.create ride
  Notify.notifyOnRideAssigned rideBooking ride
  where
    buildRide :: MonadFlow m => SRB.RideBooking -> m SRide.Ride
    buildRide rideBooking = do
      guid <- generateGUID
      shortId <- generateShortId
      now <- getCurrentTime
      let bppRideId = Id taEvent.ride_id
          otp = taEvent.otp
          driverName = taEvent.agent.name
          driverMobileNumber = taEvent.agent.phone
          driverRating = realToFrac <$> taEvent.agent.rating
          driverRegisteredAt = taEvent.agent.registered_at
          vehicleNumber = taEvent.vehicle.registration
          vehicleColor = taEvent.vehicle.color
          vehicleModel = taEvent.vehicle.model
      return
        SRide.Ride
          { id = guid,
            bookingId = rideBooking.id,
            status = SRide.NEW,
            trackingUrl = "UNKNOWN", -- TODO: Fill this field
            fare = Nothing,
            totalFare = Nothing,
            chargeableDistance = Nothing,
            vehicleVariant = rideBooking.vehicleVariant,
            createdAt = now,
            updatedAt = now,
            ..
          }
    isAssignable rideBooking = rideBooking.status `elem` [SRB.CONFIRMED, SRB.AWAITING_REASSIGNMENT]
processEvent (OnUpdate.RideStarted rsEvent) = do
  let bppBookingId = Id rsEvent.order_id
      bppRideId = Id rsEvent.ride_id
  rideBooking <- QRB.findByBPPBookingId bppBookingId >>= fromMaybeM (RideBookingDoesNotExist bppBookingId.getId)
  ride <- QRide.findByBPPRideId bppRideId >>= fromMaybeM (RideDoesNotExist bppRideId.getId)
  unless (rideBooking.status == SRB.TRIP_ASSIGNED) $ throwError (RideBookingInvalidStatus $ show rideBooking.status)
  unless (ride.status == SRide.NEW) $ throwError (RideInvalidStatus $ show ride.status)
  DB.runTransaction $ do
    QRide.updateStatus ride.id SRide.INPROGRESS
  Notify.notifyOnRideStarted rideBooking ride
processEvent (OnUpdate.RideCompleted rcEvent) = do
  let bppBookingId = Id rcEvent.order_id
      bppRideId = Id rcEvent.ride_id
  rideBooking <- QRB.findByBPPBookingId bppBookingId >>= fromMaybeM (RideBookingDoesNotExist bppBookingId.getId)
  ride <- QRide.findByBPPRideId bppRideId >>= fromMaybeM (RideDoesNotExist bppRideId.getId)
  unless (rideBooking.status == SRB.TRIP_ASSIGNED) $ throwError (RideBookingInvalidStatus $ show rideBooking.status)
  unless (ride.status == SRide.INPROGRESS) $ throwError (RideInvalidStatus $ show ride.status)
  let updRide =
        ride{status = SRide.COMPLETED,
             fare = Just $ realToFrac rcEvent.fare.value,
             totalFare = Just $ realToFrac rcEvent.total_fare.value,
             chargeableDistance = Just $ realToFrac rcEvent.chargeable_distance
            }
  DB.runTransaction $ do
    QRB.updateStatus rideBooking.id SRB.COMPLETED
    QRide.updateMultiple updRide.id updRide
  Notify.notifyOnRideCompleted rideBooking ride
processEvent (OnUpdate.RideBookingCancelled tcEvent) = do
  let bppRideBookingId = Id $ tcEvent.order_id
  rideBooking <- QRB.findByBPPBookingId bppRideBookingId >>= fromMaybeM (RideBookingDoesNotExist bppRideBookingId.getId)
  unless (isRideBookingCancellable rideBooking) $
    throwError (RideBookingInvalidStatus (show rideBooking.status))
  mbRide <- QRide.findActiveByRBId rideBooking.id
  let cancellationSource = tcEvent.cancellation_reason_id
  let searchRequestId = rideBooking.requestId
  logTagInfo ("txnId-" <> getId searchRequestId) ("Cancellation reason " <> show cancellationSource)
  rideBookingCancellationReason <- buildRideBookingCancellationReason rideBooking.id (mbRide <&> (.id)) cancellationSource
  DB.runTransaction $ do
    QRB.updateStatus rideBooking.id SRB.CANCELLED
    whenJust mbRide $ \ride -> QRide.updateStatus ride.id SRide.CANCELLED
    unless (cancellationSource == OnUpdate.ByUser) $
      QBCR.create rideBookingCancellationReason
  -- notify customer
  Notify.notifyOnRideBookingCancelled rideBooking cancellationSource
  where
    isRideBookingCancellable rideBooking =
      rideBooking.status `elem` [SRB.CONFIRMED, SRB.AWAITING_REASSIGNMENT, SRB.TRIP_ASSIGNED]
processEvent (OnUpdate.RideBookingReallocation rbrEvent) = do
  let bppRideBookingId = Id $ rbrEvent.order_id
      bppRideId = Id rbrEvent.ride_id
  rideBooking <- QRB.findByBPPBookingId bppRideBookingId >>= fromMaybeM (RideBookingDoesNotExist bppRideBookingId.getId)
  ride <- QRide.findByBPPRideId bppRideId >>= fromMaybeM (RideDoesNotExist bppRideId.getId)
  let cancellationSource = rbrEvent.cancellation_reason_id
  let searchRequestId = rideBooking.requestId
  logTagInfo ("txnId-" <> getId searchRequestId) ("Cancellation reason " <> show cancellationSource)
  rideBookingCancellationReason <- buildRideBookingCancellationReason rideBooking.id (Just ride.id) cancellationSource
  DB.runTransaction $ do
    QRB.updateStatus rideBooking.id SRB.AWAITING_REASSIGNMENT
    QRide.updateStatus ride.id SRide.CANCELLED
    unless (cancellationSource == OnUpdate.ByUser) $
      QBCR.create rideBookingCancellationReason
  -- notify customer
  Notify.notifyOnRideBookingReallocated rideBooking cancellationSource

buildRideBookingCancellationReason ::
  MonadFlow m =>
  Id SRB.RideBooking ->
  Maybe (Id SRide.Ride) ->
  OnUpdate.CancellationSource ->
  m SBCR.RideBookingCancellationReason
buildRideBookingCancellationReason rideBookingId mbRideId cancellationSource = do
  guid <- generateGUID
  return
    SBCR.RideBookingCancellationReason
      { id = guid,
        rideBookingId = rideBookingId,
        rideId = mbRideId,
        source = cancellationSource,
        reasonCode = Nothing,
        reasonStage = Nothing,
        additionalInfo = Nothing
      }
