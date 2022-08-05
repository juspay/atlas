{-# LANGUAGE DerivingStrategies #-}


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

Module      :  Types.Notification
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Types.Notification where

import Beckn.Utils.JSON
import Domain.Types.SearchRequest
import EulerHS.Prelude

data NotificationType
  = LEAD
  | CUSTOMER_APPROVED
  deriving (Generic, FromJSON, ToJSON)

data Notification a = Notification
  { _type :: NotificationType,
    payload :: a
  }
  deriving (Generic)

instance FromJSON a => FromJSON (Notification a) where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON a => ToJSON (Notification a) where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

type SearchRequestNotification = Notification SearchRequest
