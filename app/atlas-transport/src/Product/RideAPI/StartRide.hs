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

Module      :  Product.RideAPI.StartRide
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Product.RideAPI.StartRide where

import App.Types (FlowHandler)
import qualified Beckn.Storage.Esqueleto as Esq
import qualified Beckn.Types.APISuccess as APISuccess
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Utils.SlidingWindowLimiter (checkSlidingWindowLimit)
import qualified Domain.Types.Person as SP
import qualified Domain.Types.Ride as SRide
import qualified Domain.Types.RideBooking as SRB
import EulerHS.Prelude hiding (id)
import Product.BecknProvider.BP
import qualified Product.RideAPI.Handlers.StartRide as Handler
import qualified Storage.Queries.BusinessEvent as QBE
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RideBooking as QRB
import Types.API.Ride (StartRideReq (..))
import Utils.Common (withFlowHandlerAPI)

startRide :: Id SP.Person -> Id SRide.Ride -> StartRideReq -> FlowHandler APISuccess.APISuccess
startRide personId rideId req = withFlowHandlerAPI $ do
  Handler.startRideHandler handle personId (cast rideId) (req.rideOtp)
  where
    handle =
      Handler.ServiceHandle
        { findById = QPerson.findById,
          findRideBookingById = QRB.findById,
          findRideById = QRide.findById,
          startRide = startRideTransaction,
          notifyBAPRideStarted = sendRideStartedUpdateToBAP,
          rateLimitStartRide = \personId' rideId' -> checkSlidingWindowLimit (getId personId' <> "_" <> getId rideId')
        }

startRideTransaction :: EsqDBFlow m r => Id SRide.Ride -> Id SRB.RideBooking -> Id SP.Person -> m ()
startRideTransaction rideId rideBookingId driverId = Esq.runTransaction $ do
  QRide.updateStatus rideId SRide.INPROGRESS
  QRide.updateStartTime rideId
  QBE.logRideCommencedEvent (cast driverId) rideBookingId rideId
