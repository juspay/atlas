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

Module      :  Domain.Types.CancellationReason
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Domain.Types.CancellationReason where

import Beckn.Prelude
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import Servant

data CancellationStage = OnSearch | OnConfirm | OnAssign
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

instance FromHttpApiData CancellationStage where
  parseUrlPiece = parseHeader . encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = left T.pack . eitherDecode . BSL.fromStrict

newtype CancellationReasonCode = CancellationReasonCode Text
  deriving (Generic, Show, Eq, Read, ToJSON, FromJSON, ToSchema)

data CancellationReason = CancellationReason
  { reasonCode :: CancellationReasonCode,
    description :: Text,
    enabled :: Bool,
    onSearch :: Bool,
    onConfirm :: Bool,
    onAssign :: Bool
  }
  deriving (Generic, Show)

data CancellationReasonAPIEntity = CancellationReasonAPIEntity
  { reasonCode :: CancellationReasonCode,
    description :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

makeCancellationReasonAPIEntity :: CancellationReason -> CancellationReasonAPIEntity
makeCancellationReasonAPIEntity CancellationReason {..} =
  CancellationReasonAPIEntity {..}
