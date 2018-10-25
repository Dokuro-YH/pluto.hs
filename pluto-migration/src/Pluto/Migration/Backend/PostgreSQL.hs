module Pluto.Migration.Backend.PostgreSQL
  ( pgBackend
  , MigrationBackend
  ) where

import           Control.Exception              (SomeException (..), bracket,
                                                 try)
import           Control.Monad                  (void)
import           Data.String                    (fromString)
import qualified Data.Text                      as T
import           Database.PostgreSQL.Simple     (ConnectInfo (..), Connection,
                                                 close, connect,
                                                 connectPostgreSQL, execute,
                                                 execute_, fromOnly, query,
                                                 query_)
import           Database.PostgreSQL.Simple.URL (parseDatabaseUrl)

import           Pluto.Migration.Types

migrationTableName :: String
migrationTableName = "__installed_migrations__"

getTablesSql, createTableSql, dropTableSql :: String
getTablesSql =
  "SELECT tablename FROM pg_catalog.pg_tables WHERE NOT schemaname IN ('information_schema', 'pg_catalog')"
createTableSql =
  mconcat
    [ "CREATE TABLE "
    , migrationTableName
    , " (migration_id TEXT PRIMARY KEY, run_on TIMESTAMP WITHOUT TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP)"
    ]
dropTableSql = "DROP TABLE " ++ migrationTableName

-- | PostgreSQL 'MigrationBackend' constructor for DATABASE_URL
pgBackend :: String -> MigrationBackend
pgBackend url =
  MigrationBackend
    { initialized = elem migrationTableName <$> getTables url
    , createDatabaseIfNeeded = pgCreateDatabaseIfNeeded url
    , getInitMigration = pgGetInitMigration
    , getMigrations = pgGetMigrations url
    , installMigration = pgInstallMigration url
    , revertMigration = pgRevertMigration url
    }

withConn :: String -> (Connection -> IO a) -> IO a
withConn url = bracket (connectPostgreSQL $ fromString url) close

pgCreateDatabaseIfNeeded :: String -> IO ()
pgCreateDatabaseIfNeeded url = do
  result <- try (connectPostgreSQL $ fromString url)
  case result of
    Right conn             -> close conn
    Left (SomeException _) -> case changeDatabaseOfUrl "postgres" of
      Nothing -> fail "create database."
      Just (database, connectInfo) -> do
        putStrLn $ "Creating database: " ++ database
        conn <- connect connectInfo
        void $ createDatabase conn database
  where
    createDatabase conn name = execute_ conn (fromString $ "CREATE DATABASE " ++ name)
    changeDatabaseOfUrl defaultDatabase = do
      info <- parseDatabaseUrl $ fromString url
      return (connectDatabase info, info { connectDatabase = defaultDatabase })

pgGetInitMigration :: IO Migration
pgGetInitMigration = return $
  Migration
    {mId = migrationTableName, mApply = createTableSql, mRevert = dropTableSql}

pgGetMigrations :: String -> IO [String]
pgGetMigrations url =
  withConn url $ \conn ->
    fmap (T.unpack . fromOnly) <$>
    query
      conn
      (fromString $
       "SELECT migration_id FROM " ++
       migrationTableName ++
       " where migration_id <> ? ORDER BY run_on") [migrationTableName]

pgInstallMigration :: String -> Migration -> IO ()
pgInstallMigration url m =
  withConn url $ \conn -> do
    _ <- executeIfNeeded conn $ mApply m
    void $
      execute
        conn
        (fromString $
         "INSERT INTO " ++ migrationTableName ++ " (migration_id) VALUES (?)")
        [mId m]

pgRevertMigration :: String -> Migration -> IO ()
pgRevertMigration url m = withConn url $ \conn -> do
  _ <- executeIfNeeded conn $ mRevert m
  void $
    execute
      conn
      (fromString $
       "DELETE FROM " ++ migrationTableName ++ " WHERE migration_id = ?")
      [mId m]

executeIfNeeded :: Connection -> String -> IO ()
executeIfNeeded conn sql
  | null sql = return ()
  | otherwise = void <$> execute_ conn $ fromString $ dropComments sql

dropComments :: String -> String
dropComments ('-':'-':xs) = (dropWhile (/= '\n') xs)
dropComments xs           = xs

getTables :: String -> IO [String]
getTables url =
  withConn url $ \conn ->
    fmap (T.unpack . fromOnly) <$> query_ conn (fromString getTablesSql)
