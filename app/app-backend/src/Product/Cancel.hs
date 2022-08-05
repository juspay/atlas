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

Module      :  Product.Cancel
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Product.Cancel (cancel) where

import App.Types
import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as DB
import Beckn.Types.APISuccess (APISuccess (Success))
import qualified Beckn.Types.Core.Context as Context
import qualified Beckn.Types.Core.ReqTypes as Common
import qualified Beckn.Types.Core.Taxi.Cancel.Req as ReqCancel
import Beckn.Types.Id
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Ride as Ride
import qualified Domain.Types.RideBooking as SRB
import qualified Domain.Types.RideBookingCancellationReason as SBCR
import qualified ExternalAPI.Flow as ExternalAPI
import qualified Storage.Queries.Ride as QR
import qualified Storage.Queries.RideBooking as QRB
import qualified Storage.Queries.RideBookingCancellationReason as QBCR
import Types.API.Cancel as API
import Types.Error
import Utils.Common

cancel :: Id SRB.RideBooking -> Id Person.Person -> API.CancelReq -> FlowHandler CancelRes
cancel bookingId personId req = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  let bookingCancellationReasonAPI = req.bookingCancellationReason
  rideBooking <- QRB.findById bookingId >>= fromMaybeM (RideBookingDoesNotExist bookingId.getId)
  canCancelRideBooking <- isRideBookingCancellable rideBooking
  unless canCancelRideBooking $
    throwError $ RideInvalidStatus "Cannot cancel this ride"
  bapURIs <- asks (.bapSelfURIs)
  bapIDs <- asks (.bapSelfIds)
  msgId <- generateGUID
  context <- buildTaxiContext Context.CANCEL msgId Nothing bapIDs.cabs bapURIs.cabs (Just rideBooking.providerId) (Just rideBooking.providerUrl)

  when (rideBooking.status == SRB.NEW) $ throwError (RideBookingInvalidStatus "NEW")
  bppOrderId <- fromMaybeM (RideBookingFieldNotPresent "bppBookingId") rideBooking.bppBookingId
  void $ ExternalAPI.cancel rideBooking.providerUrl (Common.BecknReq context (ReqCancel.CancelReqMessage bppOrderId.getId ReqCancel.ByUser))

  rideBookingCancelationReason <- buildRideBookingCancelationReason rideBooking.id bookingCancellationReasonAPI
  DB.runTransaction
    (QBCR.create rideBookingCancelationReason)
    `rethrow` \(SQLRequestError _ _) -> RideInvalidStatus "This ride is already cancelled"
  return Success
  where
    buildRideBookingCancelationReason rideBookingId bookingCancellationReasonAPI = do
      let RideBookingCancellationReasonAPIEntity {..} = bookingCancellationReasonAPI
      id <- generateGUID
      return $
        SBCR.RideBookingCancellationReason
          { rideBookingId = rideBookingId,
            rideId = Nothing,
            source = ReqCancel.ByUser,
            reasonCode = Just reasonCode,
            reasonStage = Just reasonStage,
            additionalInfo = additionalInfo,
            ..
          }

isRideBookingCancellable :: EsqDBFlow m r => SRB.RideBooking -> m Bool
isRideBookingCancellable rideBooking
  | rideBooking.status `elem` [SRB.CONFIRMED, SRB.AWAITING_REASSIGNMENT] = pure True
  | rideBooking.status == SRB.TRIP_ASSIGNED = do
    ride <- QR.findActiveByRBId rideBooking.id >>= fromMaybeM (RideDoesNotExist rideBooking.id.getId)
    pure (ride.status == Ride.NEW)
  | otherwise = pure False
