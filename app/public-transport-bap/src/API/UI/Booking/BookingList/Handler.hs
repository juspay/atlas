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

Module      :  API.UI.Booking.BookingList.Handler
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module API.UI.Booking.BookingList.Handler where

import API.UI.Booking.BookingList.Types
import Beckn.Prelude
import Beckn.Utils.Common
import Domain.Action.UI.BookingList as BookingList
import Domain.Types.Booking
import Environment
import Tools.Auth

handler :: FlowServer API
handler = bookingList

bookingList :: PersonId -> Maybe Integer -> Maybe Integer -> FlowHandler BookingListRes
bookingList personId mbLimit mbOffset = withFlowHandlerAPI $ do
  BookingList.bookingListHandler personId mbLimit mbOffset
