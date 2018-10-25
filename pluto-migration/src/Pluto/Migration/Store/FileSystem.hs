module Pluto.Migration.Store.FileSystem
  ( FilePath
  , fsStore
  ) where

import           Control.Exception     (Exception, throw)
import           Control.Monad         (filterM, unless)
import           System.Directory      (createDirectoryIfMissing,
                                        doesDirectoryExist, doesFileExist,
                                        listDirectory)
import           System.FilePath       (takeBaseName, takeFileName, (</>))

import           Pluto.Migration.Types

newtype FilesystemStoreError =
  FilesystemStoreError String
  deriving (Show)

instance Exception FilesystemStoreError

throwFS :: String -> a
throwFS = throw . FilesystemStoreError

filenameExtension :: String
filenameExtension = ".sql"

applyFilename :: String
applyFilename = "up"

revertFilename :: String
revertFilename = "down"

fsStore :: FilePath -> MigrationStore
fsStore root = MigrationStore
  { initStore = fsSetup root
  , listMigrations = fsListMigrations root
  , lookupMigration = fsLookupMigration root
  , saveMigration = fsSaveMigration root
  }

fsSetup :: FilePath -> IO ()
fsSetup root = do
  exist <- doesDirectoryExist root
  unless exist (putStrLn $ "Creating directory: " ++ root)
  createDirectoryIfMissing False root

fsListMigrations :: FilePath -> IO [String]
fsListMigrations root = do
  exist <- doesDirectoryExist root
  if exist
    then listDirectory root >>= filterM checkIsDir
    else return []
  where
    checkIsDir path = doesDirectoryExist $ root </> path

fsLookupMigration :: FilePath -> FilePath -> IO Migration
fsLookupMigration root migrationId = do
  applySql <- readSqlFile migrationDir applyFilename
  revertSql <- readSqlFile migrationDir revertFilename
  return $ Migration { mId = fsPathToMigrationId migrationDir, mApply = applySql, mRevert = revertSql}
  where
    migrationDir = root </> migrationId
    readSqlFile path name = do
      let file = path </> name ++ filenameExtension
      fileExist <- doesFileExist file
      if fileExist
        then readFile file
        else throwFS $ "Could not found file: " ++ file

fsSaveMigration :: FilePath -> Migration -> IO ()
fsSaveMigration root m = do
  createDirectoryIfMissing True path
  saveFile applyFilename (mApply m)
  saveFile revertFilename (mRevert m)
  where
    path = root </> mId m
    saveFile :: String -> String -> IO ()
    saveFile name contents = do
      let file = path </> name ++ filenameExtension
      writeFile file contents

fsPathToMigrationId :: FilePath -> String
fsPathToMigrationId path = takeBaseName $ takeFileName path
