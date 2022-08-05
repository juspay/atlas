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

Module      :  API.Parking.Handler
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module API.Parking.Handler where

import qualified API.Parking.Booking.Handler as Booking
import API.Parking.Quotes.Handler as ConfirmQuote
import qualified API.Parking.Search.Handler as Search
import qualified API.Parking.SearchId.Quotes.Handler as GetQuote
import qualified API.Parking.Types as Parking
import App.Types
import Servant

handler :: FlowServer Parking.API
handler =
  Search.handler
    :<|> GetQuote.handler
    :<|> Booking.handler
    :<|> ConfirmQuote.handler
