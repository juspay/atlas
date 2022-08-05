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

Module      :  API.Handler
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module API.Handler where

import qualified API.Beckn.Handler as Beckn
import qualified API.Parking.Handler as Parking
import qualified API.Swagger.Handler as Swagger
import qualified API.Types as API
import App.Types
import Servant

handler :: FlowServer API.API
handler =
  ( Beckn.handler
      :<|> Parking.handler
  )
    :<|> Swagger.handler
