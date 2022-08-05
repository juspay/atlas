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

Module      :  Common
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Common where

import "app-backend" App.Routes as AbeRoutes
import Beckn.Types.Base64
import Beckn.Types.Id
import Beckn.Types.MapSearch (LatLong (..))
import qualified Beckn.Utils.SignatureAuth as HttpSig
import qualified Data.Aeson as J
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.UTF8 as B
import Data.Time.Clock.POSIX
import qualified "app-backend" Domain.Types.SearchRequest as BSearchRequest
import EulerHS.Prelude
import Network.HTTP.Types.Status
import Servant.Client
import Test.Hspec hiding (context)
import qualified Types.API.Quote as QuoteAPI
import qualified "app-backend" Types.API.Search as AppBESearch
import Utils (defaultManager, runClient')

getAppBaseUrl :: BaseUrl
getAppBaseUrl =
  BaseUrl
    { baseUrlScheme = Http,
      baseUrlHost = "localhost",
      baseUrlPort = 8013,
      baseUrlPath = "/v2"
    }

appBackendClientEnv :: ClientEnv
appBackendClientEnv = mkClientEnv defaultManager getAppBaseUrl

callAppBackend :: (Show a) => ClientM a -> IO a
callAppBackend = runClient' appBackendClientEnv

searchServices ::
  Text ->
  AppBESearch.SearchReq ->
  ClientM AppBESearch.SearchRes
searchServices = client (Proxy :: Proxy AbeRoutes.SearchAPI)

getQuotes :: Id BSearchRequest.SearchRequest -> Text -> ClientM QuoteAPI.GetQuotesRes
getQuotes = client (Proxy :: Proxy AbeRoutes.QuoteAPI)

gatewayBaseUrl :: BaseUrl
gatewayBaseUrl =
  BaseUrl
    { baseUrlScheme = Http,
      baseUrlHost = "localhost",
      baseUrlPort = 8015,
      baseUrlPath = "/v1"
    }

gatewayClientEnv :: ClientEnv
gatewayClientEnv = mkClientEnv defaultManager gatewayBaseUrl

callGateway :: (Show a) => ClientM a -> IO a
callGateway = runClient' gatewayClientEnv

address :: AppBESearch.SearchReqAddress
address =
  AppBESearch.SearchReqAddress
    { door = Just "#817",
      building = Just "Juspay Apartments",
      street = Just "27th Main",
      area = Just "8th Block Koramangala",
      city = Just "Bangalore",
      country = Just "India",
      areaCode = Just "560047",
      state = Just "Karnataka"
    }

searchReq :: AppBESearch.SearchReq
searchReq =
  AppBESearch.OneWaySearch $
    AppBESearch.OneWaySearchReq
      { origin = AppBESearch.SearchReqLocation address $ LatLong 10.0739 76.2733,
        destination = AppBESearch.SearchReqLocation address $ LatLong 10.5449 76.4356
      }

verifyError :: Int -> B.ByteString -> Either ClientError a -> IO ()
verifyError expectedCode expectedMessage serverResponse = do
  case serverResponse of
    Left (FailureResponse _ response) -> do
      statusCode (responseStatusCode response) `shouldBe` expectedCode
      BL.toStrict (responseBody response)
        `shouldSatisfy` (expectedMessage `B.isInfixOf`)
    _ -> expectationFailure ("Expected " <> B.toString expectedMessage <> " error.")

privateKey :: Base64
privateKey = "hrBkJ7AAAIQLmBM/tyhUg01dZRU7+DMQwLxjGlGmWow="

signRequest :: ToJSON req => req -> POSIXTime -> Text -> Text -> ByteString
signRequest req now orgId keyId =
  let body = BL.toStrict $ J.encode req
      bodyHash = HttpSig.becknSignatureHash body
      headers = [("(created)", ""), ("(expires)", ""), ("digest", "")]
      params = HttpSig.mkSignatureParams orgId keyId now 600 HttpSig.Ed25519
      signature = fromJust $ HttpSig.sign privateKey params bodyHash headers
   in HttpSig.encode $ HttpSig.SignaturePayload signature params
