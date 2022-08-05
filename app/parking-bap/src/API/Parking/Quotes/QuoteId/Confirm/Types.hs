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

Module      :  API.Parking.Quotes.QuoteId.Confirm.Types
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module API.Parking.Quotes.QuoteId.Confirm.Types where

import Beckn.Prelude
import Beckn.Types.Id
import Domain.Booking (Booking)
import Servant
import Tools.Auth

type API =
  "confirm"
    :> PostQuoteConfirmAPI

type PostQuoteConfirmAPI =
  TokenAuth
    :> ReqBody '[JSON] PostQuoteConfirmReq
    :> Post '[JSON] PostQuoteConfirmRes

data PostQuoteConfirmReq = PostQuoteConfirmReq
  { requestorNumber :: Text,
    vehicleNumber :: Text,
    requestorName :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

newtype PostQuoteConfirmRes = PostQuoteConfirmRes
  { bookingId :: Id Booking
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, ToSchema)
