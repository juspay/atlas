{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}


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

Module      :  Main
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Main where

import Beckn.External.Encryption (DbHash (..), DbHashable (..))
import Crypto.Saltine.Core.Box (newKeypair)
import Crypto.Saltine.Core.SecretBox (newKey)
import qualified Data.Aeson as A
import Data.Binary (Binary, encode)
import qualified Data.Vector as V (fromList)
import Passetto.Crypto (encryptKey, encryptMasterKey, passwordFromText, sodiumInit)
import Passetto.JsonEncryption (encryptPayload)
import Passetto.KeysContext (mkKeysContext)
import System.Environment (getArgs)
import Text.Hex (encodeHex, strictByteString)
import Universum

main :: IO ()
main = getArgs >>= doit
  where
    doit [ns, ps, pid, pmobile]
      | Just n <- readMaybe ns = main_ n (fromString ps) (fromString pid) (fromString pmobile)
    doit _ = putStrLn @Text "Usage: geninis num_of_keys password person_id person_mobile_number"

main_ :: Int -> Text -> Text -> Text -> IO ()
main_ n p pid pmo = do
  sodiumInit
  m <- newKey
  em <- encryptMasterKey (passwordFromText p) m
  ks <- replicateM n newKeypair
  eks <- mapM (encryptKey m) ks
  writeFile "passetto_init_data.sql" $
    unlines $
      ("INSERT INTO \"Passetto\".\"Master\" (key) values (decode('" <> b2hexs em <> "', 'hex'));") :
      map (\k -> "INSERT INTO \"Passetto\".\"Keys\" (encryptedKeyPair) values(decode('" <> b2hexs k <> "', 'hex'));") eks
  ek <- mkKeysContext (V.fromList ks) >>= runReaderT (encryptPayload $ "S" <> renderJSon (A.String pmo))
  let genAddEnc table =
        writeFile (toString table <> "_add_encrypted_phone.sql") $
          "UPDATE atlas_" <> table <> ".person SET mobile_number_encrypted = '" <> ek
            <> "', mobile_number_hash = decode('"
            <> encodeHex (unDbHash $ evalDbHash pmo)
            <> "', 'hex') where id = '"
            <> pid
            <> "';"
  genAddEnc "app"
  genAddEnc "transporter"
  where
    b2hexs :: Binary a => a -> Text
    b2hexs = encodeHex . strictByteString . encode
    renderJSon = decodeUtf8 . A.encode
