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

Module      :  Utils.PostgreSQLSimple

Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Utils.PostgreSQLSimple
  ( postgreSQLSimpleExecute,
    postgreSQLSimpleQuery,
    fromFieldRead,
  )
where

import Beckn.Storage.DB.Config (DBConfig (..))
import Beckn.Utils.Common
import Control.Exception (Handler (..), catches)
import Data.Pool (withResource)
import Data.Text (pack)
import Data.Text.Encoding (decodeUtf8)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (decodeUtf8, encodeUtf8, pack, (.=))
import EulerHS.Types (SqlConn (PostgresPool), mkPostgresPoolConfig)
import Types.Error

postgreSQLSimpleExecute :: (DBFlow m r, ToRow row) => Query -> row -> m Int64
postgreSQLSimpleExecute q qargs = do
  logQuery q qargs
  withPostgreSQLSimple (\conn -> execute conn q qargs)

postgreSQLSimpleQuery ::
  ( DBFlow m r,
    FromRow res,
    ToRow row
  ) =>
  Query ->
  row ->
  m [res]
postgreSQLSimpleQuery q qargs = do
  logQuery q qargs
  withPostgreSQLSimple (\conn -> query conn q qargs)

withPostgreSQLSimple :: (DBFlow m r) => (Connection -> IO a) -> m a
withPostgreSQLSimple f = do
  DBConfig {..} <- asks (.dbCfg)
  pool <-
    L.getOrInitSqlConn (mkPostgresPoolConfig connTag pgConfig poolConfig)
      >>= checkDBError
      >>= \case
        PostgresPool _connTag pool -> pure pool
        _ -> throwError NotPostgresBackend
  res <-
    liftIO . withResource pool $
      runPostgresqlSimple . fmap Right . f
  res & fromEitherM (SQLRequestError "postgresql-simple")

runPostgresqlSimple :: IO (Either Text a) -> IO (Either Text a)
runPostgresqlSimple = (`catches` handlers)
  where
    handlers =
      [ Handler (\(e :: FormatError) -> pure . Left . pack $ fmtMessage e),
        Handler (\(e :: ResultError) -> pure . Left . pack $ showResultError e),
        Handler (\(e :: QueryError) -> pure . Left . pack $ qeMessage e),
        Handler
          ( \(e :: SqlError) ->
              pure . Left . decodeUtf8 $
                sqlErrorMsg e <> " (" <> sqlErrorDetail e <> ") (" <> sqlErrorHint e <> ")"
          )
      ]

showResultError :: ResultError -> String
showResultError (Incompatible sqlType _ _ hType msg) = "sql incompatible: " <> msg <> " (" <> hType <> " ~ " <> sqlType <> ")"
showResultError (UnexpectedNull _ _ field _ msg) = "sql unexpected null: " <> msg <> " @ " <> field
showResultError (ConversionFailed sqlType _ _ hType msg) = "sql conversion failed" <> msg <> " (" <> hType <> " ~ " <> sqlType <> ")"

logQuery :: (DBFlow m r, ToRow q) => Query -> q -> m ()
logQuery q qargs =
  withPostgreSQLSimple (\conn -> decodeUtf8 <$> formatQuery conn q qargs)
    >>= logTagDebug "raw sql query"

fromFieldRead ::
  (Typeable b, Read b) =>
  String ->
  Field ->
  Maybe ByteString ->
  Conversion b
fromFieldRead typeName field bs = fromField field bs >>= maybe err pure . readMaybe
  where
    err = returnError ConversionFailed field $ "Cannot read " <> typeName
