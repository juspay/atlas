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

Module      :  Utils
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Utils where

import Beckn.Types.Amount
import qualified Beckn.Types.Core.Migration.DecimalValue as Core
import Core.Common.Price
import Data.String.Conversions
import Relude hiding (id, state)

buildPrice :: (Integral a) => a -> Price
buildPrice x =
  Price
    { currency = "INR",
      value = Core.convertAmountToDecimalValue $ Amount $ fromIntegral x
    }

buildPriceAmount :: Amount -> Price
buildPriceAmount x =
  Price
    { currency = "INR",
      value = Core.convertAmountToDecimalValue x
    }

validateUnique :: Text -> [a] -> Either Text a
validateUnique _ [x] = Right x
validateUnique entity [] = Left $ "empty " <> entity <> " list"
validateUnique entity _ = Left $ "expected one " <> entity
