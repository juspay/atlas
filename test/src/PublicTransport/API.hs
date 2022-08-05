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

Module      :  PublicTransport.API
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module PublicTransport.API where

import qualified "public-transport-bap" API.UI.Types as Bap
import Beckn.Prelude hiding (Proxy)
import Beckn.Types.APISuccess (APISuccess)
import Beckn.Types.App (RegToken)
import Beckn.Types.Id
import Data.Proxy
import qualified "public-transport-bap" Domain.Action.UI.QuoteConfirm as Bap
import qualified "public-transport-bap" Domain.Action.UI.Quotes as Bap
import qualified "public-transport-bap" Domain.Types.Booking.API as Bap
import qualified "public-transport-bap" Domain.Types.Booking.Type as Bap
import qualified "public-transport-bap" Domain.Types.Quote as Bap
import qualified "public-transport-bap" Domain.Types.Search as Bap
import Servant hiding (Proxy)
import Servant.Client

userToken :: Text
userToken = "ea37f941-427a-4085-a7d0-96240f166672"

bookingClientM :: RegToken -> Id Bap.Booking -> ClientM Bap.BookingAPIEntity
triggerStatusClientM :: RegToken -> Id Bap.Booking -> ClientM APISuccess
getQuotesClientM :: Id Bap.Search -> RegToken -> ClientM Bap.GetQuotesRes
quoteConfirmClientM :: RegToken -> Id Bap.Quote -> Bap.QConfirmReq -> ClientM Bap.QConfirmRes
(_ :<|> bookingClientM :<|> triggerStatusClientM) :<|> getQuotesClientM :<|> quoteConfirmClientM = client (Proxy @Bap.API)
