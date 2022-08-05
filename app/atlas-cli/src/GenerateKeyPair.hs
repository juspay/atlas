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

Module      :  GenerateKeyPair
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module GenerateKeyPair where

import Beckn.Types.Credentials
import qualified Beckn.Utils.SignatureAuth as HttpSig
import qualified EulerHS.Language as L
import EulerHS.Prelude

data GenerateKeyPairResponse = GenerateKeyPairResponse
  { privateKey :: PrivateKey,
    publicKey :: PublicKey
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

generateKeyPair :: L.Flow GenerateKeyPairResponse
generateKeyPair = do
  L.logInfo @Text "GenerateKeyPair" "Generating random key pair."
  L.runIO HttpSig.generateKeyPair <&> uncurry GenerateKeyPairResponse
