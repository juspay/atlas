{-# LANGUAGE OverloadedLabels #-}


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

Module      :  PrepareDataForLoadTest

Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module PrepareDataForLoadTest
  ( prepareDataForLoadTest,
    defaultPrivateKey,
    cleanupData,
    runK6Script,
  )
where

import Beckn.Types.Base64
import qualified Beckn.Types.Core.Context as API
import qualified Beckn.Types.Core.ReqTypes as API
import qualified Beckn.Types.Core.Taxi.API.Search as API
import qualified Beckn.Types.Core.Taxi.Search as API
import Beckn.Utils.Example (Example (example))
import qualified Beckn.Utils.SignatureAuth as S
import qualified Data.Aeson as J
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Time.Clock.POSIX as Time
import qualified EulerHS.Language as L
import EulerHS.Prelude
import System.Directory (removeFile)

data RequestForLoadTest = RequestForLoadTest
  { rawRequest :: !Text,
    signature :: !Text
  }
  deriving (Show, ToJSON, FromJSON, Generic)

defaultPrivateKey :: ByteString
defaultPrivateKey = "hrBkJ7AAAIQLmBM/tyhUg01dZRU7+DMQwLxjGlGmWow="

prepareDataForLoadTest :: ByteString -> Int -> Text -> L.Flow ()
prepareDataForLoadTest privateKey nmbOfReq filePath = do
  reqs <- replicateM nmbOfReq $ do
    request <- generateSearchRequest
    now <- L.runIO Time.getPOSIXTime
    pure $ do
      let body = LBS.toStrict $ J.encode request
      let bodyHash = S.becknSignatureHash body
      let headers = [("(created)", ""), ("(expires)", ""), ("digest", "")]
      let signatureParams = S.mkSignatureParams "JUSPAY.MOBILITY.APP.UAT.1" "juspay-mobility-bap-1-key" now 600 S.Ed25519
      signature <- S.sign (Base64 $ Base64.decodeLenient privateKey) signatureParams bodyHash headers
      pure $ RequestForLoadTest (decodeUtf8 body) (decodeUtf8 $ S.encode $ S.SignaturePayload signature signatureParams)
  L.runIO . writeFile (T.unpack filePath) . decodeUtf8 . J.encode . catMaybes $ reqs

cleanupData :: Text -> L.Flow ()
cleanupData path = do
  L.logInfo @Text "GenerateRequestsForLoadTest" "Cleaning up..."
  L.runIO . removeFile . T.unpack $ path

runK6Script :: Text -> Text -> Int -> L.Flow String
runK6Script url filePath nmbOfReq = do
  L.logInfo @Text "GenerateRequestsForLoadTest" "Start K6 script..."
  L.runSysCmd $
    "k6 run -e LOAD_TEST_URL="
      <> T.unpack url
      <> " -e FILE_PATH="
      <> T.unpack filePath
      <> " ./dev/load-test/script.js"
      <> " -e N_REQ="
      <> show nmbOfReq

generateSearchRequest :: L.Flow API.SearchReq
generateSearchRequest = do
  txnId <- L.generateGUID
  let context = example @API.Context & #message_id .~ txnId
  let intent = example @API.SearchMessage
  pure $ API.BecknReq context intent
