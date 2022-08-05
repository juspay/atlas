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

Module      :  Product.Services.GoogleMaps
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Product.Services.GoogleMaps where

import App.Types (FlowHandler)
import qualified Beckn.External.GoogleMaps.Client as ClientGoogleMaps
import qualified Beckn.External.GoogleMaps.Types as GoogleMaps
import Beckn.Types.Id
import qualified Domain.Types.Person as SP
import EulerHS.Prelude
import Utils.Common (withFlowHandlerAPI)

autoComplete :: Id SP.Person -> Text -> Text -> Integer -> Text -> FlowHandler GoogleMaps.SearchLocationResp
autoComplete _ input location radius lang = withFlowHandlerAPI $ do
  url <- asks (.googleMapsUrl)
  apiKey <- asks (.googleMapsKey)
  let components = "country:in"
  ClientGoogleMaps.autoComplete url apiKey input location radius components lang

placeDetails :: Id SP.Person -> Text -> FlowHandler GoogleMaps.PlaceDetailsResp
placeDetails _ placeId = withFlowHandlerAPI $ do
  url <- asks (.googleMapsUrl)
  apiKey <- asks (.googleMapsKey)
  let fields = "geometry"
  ClientGoogleMaps.placeDetails url apiKey placeId fields

getPlaceName :: Id SP.Person -> Text -> FlowHandler GoogleMaps.GetPlaceNameResp
getPlaceName _ latLng = withFlowHandlerAPI $ do
  url <- asks (.googleMapsUrl)
  apiKey <- asks (.googleMapsKey)
  ClientGoogleMaps.getPlaceName url latLng apiKey
