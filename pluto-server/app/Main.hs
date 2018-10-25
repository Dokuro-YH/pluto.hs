{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards    #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

module Main where

import           Configuration.Dotenv       (load, loadFile)
import           Configuration.Dotenv.Types (defaultConfig)
import           Control.Exception          (SomeException (..), catch)
import           Data.Maybe                 (fromMaybe)
import           System.Environment         (lookupEnv)

import           Pluto.Migration
import           Pluto.Server

main :: IO ()
main = do
  dotenv
  port <- readEnv "PORT" (sPort defaultSettings)
  databaseUrl <- fromMaybe (sDatabaseUrl defaultSettings) <$> lookupEnv "DATABASE_URL"
  migrationDir <- fromMaybe "./migrations" <$> lookupEnv "MIGRATION_DIR"
  setupDatabase databaseUrl migrationDir
  server $ PlutoSettings port databaseUrl
  where

setupDatabase url dir = do
  ensureInitializeBackend backend
  installPendingMigrations backend store
  where
    backend = pgBackend url
    store = fsStore dir

readEnv :: Read a => String -> a -> IO a
readEnv name def' = do
  ma <- lookupEnv name
  case ma of
    Nothing -> return def'
    Just sp ->
      case reads sp of
        ((a, _):_) -> return a
        _          -> fail $ "Invalid " ++ name ++ " value: " ++ sp

dotenv :: IO ()
dotenv = (loadFile defaultConfig >>= load False) `catch` (\(SomeException _) -> return ())
