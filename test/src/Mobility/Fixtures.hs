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

Module      :  Mobility.Fixtures
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Mobility.Fixtures where

import "app-backend" App.Routes as AbeRoutes
import "atlas-transport" App.Routes as TbeRoutes
import Beckn.External.FCM.Types
import Beckn.Types.APISuccess
import Beckn.Types.App
import Beckn.Types.Id
import Beckn.Types.MapSearch (LatLong (..))
import Beckn.Types.Time (Seconds)
import Data.Time
import qualified "app-backend" Domain.Types.CancellationReason as AbeCRC
import qualified "app-backend" Domain.Types.Person as TPerson
import qualified "app-backend" Domain.Types.Quote as BQuote
import qualified "app-backend" Domain.Types.RegistrationToken as AppSRT
import qualified "app-backend" Domain.Types.Ride as BRide
import qualified "atlas-transport" Domain.Types.Ride as TRide
import qualified "app-backend" Domain.Types.RideBooking as BRB
import qualified "atlas-transport" Domain.Types.RideBooking as TRB
import qualified "app-backend" Domain.Types.SearchRequest as BSearchRequest
import EulerHS.Prelude
import Servant hiding (Context)
import Servant.Client
import qualified Types.API.Cancel as CancelAPI
import qualified Types.API.Confirm as ConfirmAPI
import qualified "atlas-transport" Types.API.Driver as DriverAPI
import qualified "app-backend" Types.API.Feedback as AppFeedback
import "atlas-transport" Types.API.Location
import qualified "app-backend" Types.API.Registration as Reg
import qualified "atlas-transport" Types.API.Ride as RideAPI
import qualified "app-backend" Types.API.RideBooking as AppRideBooking
import qualified "atlas-transport" Types.API.RideBooking as TRideBookingAPI
import qualified "app-backend" Types.API.Search as AppBESearch
import qualified "app-backend" Types.API.Serviceability as AppServ

timeBetweenLocationUpdates :: Seconds
timeBetweenLocationUpdates = 1

defaultAddress :: AppBESearch.SearchReqAddress
defaultAddress =
  AppBESearch.SearchReqAddress
    { door = Nothing,
      building = Nothing,
      street = Nothing,
      area = Just "Edappally",
      city = Just "Kochi",
      country = Just "India",
      areaCode = Just "",
      state = Just "Kerala"
    }

bapTransporterName :: Text
bapTransporterName = "[A] Transporter #1"

getFutureTime :: IO UTCTime
getFutureTime =
  -- Generate a time 2 hours in to the future else booking will fail
  addUTCTime 7200 <$> getCurrentTime

cancelRide :: Id BRB.RideBooking -> Text -> CancelAPI.CancelReq -> ClientM CancelAPI.CancelRes
cancelRide = client (Proxy :: Proxy AbeRoutes.CancelAPI)

rideStart :: Text -> Id TRide.Ride -> RideAPI.StartRideReq -> ClientM APISuccess
rideEnd :: Text -> Id TRide.Ride -> ClientM APISuccess
rideCancel :: Text -> Id TRide.Ride -> RideAPI.CancelRideReq -> ClientM APISuccess
_ :<|> rideStart :<|> rideEnd :<|> rideCancel = client (Proxy :: Proxy TbeRoutes.RideAPI)

getDriverInfo :: Text -> ClientM DriverAPI.DriverInformationRes
setDriverOnline :: Text -> Bool -> ClientM APISuccess
( _
    :<|> _
    :<|> _
    :<|> _
    :<|> _
  )
  :<|> ( setDriverOnline
           :<|> _
           :<|> ( getDriverInfo
                    :<|> _
                  )
         ) = client (Proxy :: Proxy TbeRoutes.DriverAPI)

rideRespond :: Id TRB.RideBooking -> Text -> TRideBookingAPI.SetDriverAcceptanceReq -> ClientM TRideBookingAPI.SetDriverAcceptanceRes
rideRespond rideBookingId = rideResp
  where
    _ :<|> driver_rb_path = client (Proxy :: Proxy TbeRoutes.RideBookingAPI)
    rideResp :<|> _ = driver_rb_path rideBookingId

getNotificationInfo :: Id TRB.RideBooking -> Text -> ClientM TRideBookingAPI.GetRideInfoRes
getNotificationInfo rideBookingId = getNotif
  where
    _ :<|> driver_rb_path = client (Proxy :: Proxy TbeRoutes.RideBookingAPI)
    _ :<|> getNotif = driver_rb_path rideBookingId

buildAppCancelReq :: AbeCRC.CancellationStage -> CancelAPI.CancelReq
buildAppCancelReq stage =
  CancelAPI.CancelReq
    { bookingCancellationReason = CancelAPI.RideBookingCancellationReasonAPIEntity (AbeCRC.CancellationReasonCode "OTHER") stage Nothing
    }

appConfirmRide :: Text -> Id BSearchRequest.SearchRequest -> Id BQuote.Quote -> ClientM ConfirmAPI.ConfirmRes
appConfirmRide = client (Proxy :: Proxy AbeRoutes.ConfirmAPI)

appFeedback :: Text -> AppFeedback.FeedbackReq -> ClientM APISuccess
appFeedback = client (Proxy :: Proxy AbeRoutes.FeedbackAPI)

callAppFeedback :: Int -> Id BRide.Ride -> ClientM APISuccess
callAppFeedback ratingValue rideId =
  let request =
        AppFeedback.FeedbackReq
          { rideId = rideId,
            rating = ratingValue
          }
   in appFeedback appRegistrationToken request

tRideBookingStatus :: Id TRB.RideBooking -> Text -> ClientM TRideBookingAPI.RideBookingStatusRes
tRideBookingList :: Text -> Maybe Integer -> Maybe Integer -> Maybe Bool -> ClientM TRideBookingAPI.RideBookingListRes
(tRideBookingStatus :<|> tRideBookingList :<|> _) :<|> _ = client (Proxy :: Proxy TbeRoutes.RideBookingAPI)

appRideBookingStatus :: Id BRB.RideBooking -> Text -> ClientM AppRideBooking.RideBookingStatusRes
appRideBookingList :: Text -> Maybe Integer -> Maybe Integer -> Maybe Bool -> ClientM AppRideBooking.RideBookingListRes
appRideBookingStatus :<|> appRideBookingList = client (Proxy :: Proxy AbeRoutes.RideBookingAPI)

buildStartRideReq :: Text -> RideAPI.StartRideReq
buildStartRideReq otp =
  RideAPI.StartRideReq
    { RideAPI.rideOtp = otp
    }

originServiceability :: RegToken -> AppServ.ServiceabilityReq -> ClientM AppServ.ServiceabilityRes
originServiceability regToken = origin
  where
    origin :<|> _ = client (Proxy :: Proxy AbeRoutes.ServiceabilityAPI) regToken

destinationServiceability :: RegToken -> AppServ.ServiceabilityReq -> ClientM AppServ.ServiceabilityRes
destinationServiceability regToken = destination
  where
    _ :<|> destination = client (Proxy :: Proxy AbeRoutes.ServiceabilityAPI) regToken

updateLocation :: RegToken -> NonEmpty Waypoint -> ClientM APISuccess
(_ :<|> updateLocation) = client (Proxy @LocationAPI)

buildUpdateLocationRequest :: NonEmpty LatLong -> IO (NonEmpty Waypoint)
buildUpdateLocationRequest pts = do
  now <- getCurrentTime
  pure $
    flip fmap pts $ \ll ->
      Waypoint
        { pt = ll,
          ts = now,
          acc = Nothing
        }

appRegistrationToken :: Text
appRegistrationToken = "ea37f941-427a-4085-a7d0-96240f166672"

driverToken1 :: Text
driverToken1 = "ca05cf3c-c88b-4a2f-8874-drivertoken1"

driverToken2 :: Text
driverToken2 = "ca05cf3c-c88b-4a2f-8874-drivertoken2"

testVehicleId :: Text
testVehicleId = "0c1cd0bc-b3a4-4c6c-811f-900ccf4dfb94"

testDriverId1 :: Id TPerson.Person
testDriverId1 = Id "6bc4bc84-2c43-425d-8853-22f47driver1"

testDriverId2 :: Id TPerson.Person
testDriverId2 = Id "6bc4bc84-2c43-425d-8853-22f47driver2"

appAuth :: Reg.AuthReq -> ClientM Reg.AuthRes
appVerify :: Id AppSRT.RegistrationToken -> Reg.AuthVerifyReq -> ClientM Reg.AuthVerifyRes
appReInitiateLogin :: Id AppSRT.RegistrationToken -> ClientM Reg.ResendAuthRes
logout :: RegToken -> ClientM APISuccess
appAuth
  :<|> appVerify
  :<|> appReInitiateLogin
  :<|> logout =
    client (Proxy :: Proxy AbeRoutes.RegistrationAPI)

mkAuthReq :: Reg.AuthReq
mkAuthReq =
  Reg.AuthReq
    { mobileNumber = "9000090000",
      mobileCountryCode = "+91"
    }

mkAuthVerifyReq :: Reg.AuthVerifyReq
mkAuthVerifyReq =
  Reg.AuthVerifyReq
    { otp = "7891",
      deviceToken = FCMRecipientToken "AN_DEV_TOKEN"
    }

initiateAuth :: ClientM Reg.AuthRes
initiateAuth = appAuth mkAuthReq

verifyAuth :: Id AppSRT.RegistrationToken -> ClientM Reg.AuthVerifyRes
verifyAuth tokenId = appVerify tokenId mkAuthVerifyReq

getTransporterBaseUrl :: BaseUrl
getTransporterBaseUrl =
  BaseUrl
    { baseUrlScheme = Http,
      baseUrlHost = "localhost",
      baseUrlPort = 8014,
      baseUrlPath = "/v2"
    }
