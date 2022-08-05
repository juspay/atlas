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

Module      :  Product.Call
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Product.Call where

import App.Types
import Beckn.External.Encryption (decrypt)
import Beckn.External.Exotel.Flow (initiateCall)
import Beckn.External.Exotel.Types
import Beckn.Storage.Esqueleto (runTransaction)
import Beckn.Types.Core.Ack
import Beckn.Types.Id
import Data.Text
import qualified Data.Text as T
import qualified Domain.Types.CallStatus as SCS
import qualified Domain.Types.Person as SP
import qualified Domain.Types.Ride as SRide
import EulerHS.Prelude
import Servant.Client (BaseUrl (..))
import qualified Storage.Queries.CallStatus as QCallStatus
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RideBooking as QRB
import qualified Storage.Queries.RiderDetails as QRD
import qualified Types.API.Call as CallAPI
import Types.Error
import Utils.Common

initiateCallToCustomer :: Id SRide.Ride -> Id SP.Person -> FlowHandler CallAPI.CallRes
initiateCallToCustomer rideId _ = withFlowHandlerAPI $ do
  ride <-
    QRide.findById rideId
      >>= fromMaybeM (RideDoesNotExist rideId.getId)
  rideBooking <-
    QRB.findById ride.bookingId
      >>= fromMaybeM (RideBookingNotFound ride.bookingId.getId)
  riderDetails <-
    QRD.findById rideBooking.riderId
      >>= fromMaybeM (RiderDetailsNotFound rideBooking.riderId.getId)
  requestorPhone <- decrypt riderDetails.mobileNumber
  driverPhone <- getDriverPhone ride
  callbackUrl <- buildCallbackUrl
  callId <- generateGUID
  let attachments = ExotelAttachments {callId = getId callId, rideId = strip (getId rideId)}
  exotelResponse <- initiateCall requestorPhone driverPhone callbackUrl attachments
  logTagInfo ("RideId:" <> getId rideId) "Call initiated from driver to customer."
  let rId = strip (getId rideId)
  callStatus <- buildCallStatus (Id rId) callId exotelResponse
  runTransaction $ QCallStatus.create callStatus
  return $ CallAPI.CallRes callId
  where
    buildCallbackUrl = do
      bapUrl <- asks (.exotelCallbackUrl)
      let rideid = T.unpack (strip (getId rideId))
      return $
        bapUrl
          { baseUrlPath = baseUrlPath bapUrl <> "/driver/ride/" <> rideid <> "/call/statusCallback"
          }

    buildCallStatus rideid callId exoResponse = do
      now <- getCurrentTime
      return $
        SCS.CallStatus
          { id = callId,
            exotelCallSid = exoResponse.exoCall.exoSid.getExotelCallSID,
            rideId = rideid,
            status = exoResponse.exoCall.exoStatus,
            conversationDuration = 0,
            recordingUrl = Nothing,
            createdAt = now
          }

callStatusCallback :: Id SRide.Ride -> CallAPI.CallCallbackReq -> FlowHandler CallAPI.CallCallbackRes
callStatusCallback _ req = withFlowHandlerAPI $ do
  let callId = Id req.customField.callId
  _ <- QCallStatus.findById callId >>= fromMaybeM CallStatusDoesNotExist
  runTransaction $ QCallStatus.updateCallStatus callId req
  return Ack

getCallStatus :: Id SRide.Ride -> Id SCS.CallStatus -> Id SP.Person -> FlowHandler CallAPI.GetCallStatusRes
getCallStatus _ callStatusId _ = withFlowHandlerAPI $ do
  QCallStatus.findById callStatusId >>= fromMaybeM CallStatusDoesNotExist <&> SCS.makeCallStatusAPIEntity

getDriverPhone :: (EsqDBFlow m r, EncFlow m r) => SRide.Ride -> m Text
getDriverPhone ride = do
  let driverId = ride.driverId
  driver <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  phonenum <- SP.getPersonNumber driver
  phonenum & fromMaybeM (InternalError "Driver has no phone number.")
