{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}


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
import Beckn.External.Encryption
import Beckn.External.Exotel.Flow
import Beckn.External.Exotel.Types
import Beckn.Storage.Esqueleto (runTransaction)
import Beckn.Types.Common
import Beckn.Types.Core.Ack
import Beckn.Types.Id
import Beckn.Utils.Logging
import Data.Semigroup
import Data.Text
import qualified Data.Text as T
import Domain.Types.CallStatus
import Domain.Types.Person as Person
import qualified Domain.Types.Ride as SRide
import EulerHS.Prelude hiding (id)
import Servant.Client (BaseUrl (..))
import qualified Storage.Queries.CallStatus as QCallStatus
import Storage.Queries.Person as Person
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RideBooking as QRB
import qualified Types.API.Call as CallAPI
import Types.Error
import Utils.Common

-- | Try to initiate a call customer -> driver
initiateCallToDriver :: Id SRide.Ride -> Id Person.Person -> FlowHandler CallAPI.CallRes
initiateCallToDriver rideId personId =
  withFlowHandlerAPI . withPersonIdLogTag personId $ do
    (customerPhone, providerPhone) <- getCustomerAndDriverPhones rideId
    callbackUrl <- buildCallbackUrl
    callId <- generateGUID
    let attachments = ExotelAttachments {callId = getId callId, rideId = strip (getId rideId)}
    exotelResponse <- initiateCall customerPhone providerPhone callbackUrl attachments
    logTagInfo ("RideId: " <> getId rideId) "Call initiated from customer to driver."
    callStatus <- buildCallStatus rideId callId exotelResponse
    runTransaction $ QCallStatus.create callStatus
    return $ CallAPI.CallRes callId
  where
    buildCallbackUrl = do
      bapUrl <- asks (.exotelCallbackUrl)
      let id = T.unpack (strip (getId rideId))
      return $
        bapUrl
          { baseUrlPath = baseUrlPath bapUrl <> "/ride/" <> id <> "/call/statusCallback"
          }

    buildCallStatus id callId exotelResponse = do
      now <- getCurrentTime
      return $
        CallStatus
          { id = callId,
            exotelCallSid = exotelResponse.exoCall.exoSid.getExotelCallSID,
            rideId = id,
            status = exotelResponse.exoCall.exoStatus,
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

getCallStatus :: Id SRide.Ride -> Id CallStatus -> Id Person -> FlowHandler CallAPI.GetCallStatusRes
getCallStatus _ callStatusId _ = withFlowHandlerAPI $ do
  QCallStatus.findById callStatusId >>= fromMaybeM CallStatusDoesNotExist <&> makeCallStatusAPIEntity

getPerson :: (EsqDBFlow m r, EncFlow m r) => SRide.Ride -> m Person
getPerson ride = do
  rideBooking <- QRB.findById ride.bookingId >>= fromMaybeM (RideBookingNotFound ride.bookingId.getId)
  let personId = rideBooking.riderId
  Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)

-- | Get person's mobile phone
getPersonPhone :: EncFlow m r => Person -> m Text
getPersonPhone Person {..} = do
  decMobNum <- mapM decrypt mobileNumber
  let phonenum = (<>) <$> mobileCountryCode <*> decMobNum
  phonenum & fromMaybeM (InternalError "Customer has no phone number.")

-- | Returns phones pair or throws an error
getCustomerAndDriverPhones :: (EncFlow m r, EsqDBFlow m r) => Id SRide.Ride -> m (Text, Text)
getCustomerAndDriverPhones rideId = do
  ride <-
    QRide.findById rideId
      >>= fromMaybeM (RideDoesNotExist rideId.getId)
  person <- getPerson ride
  customerPhone <- getPersonPhone person
  return (customerPhone, ride.driverMobileNumber)
