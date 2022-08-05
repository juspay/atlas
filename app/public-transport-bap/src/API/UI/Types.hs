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

Module      :  API.UI.Types
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module API.UI.Types where

import qualified API.UI.Booking.Types as Booking
import qualified API.UI.QuoteConfirm.Types as QuoteConfirm
import API.UI.SearchId.Quotes.Types as Quotes
import Servant

type API =
  "ui"
    :> ( Booking.API
           :<|> Quotes.API
           :<|> QuoteConfirm.API
       )
