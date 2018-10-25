module Pluto.Migration
  ( ensureInitializeBackend
  , listPendingMigrations
  , installPendingMigrations
  , revertInstalledMigrations
  , redoInstalledMigrations
  , generateMigration
  , module Pluto.Migration.Backend.PostgreSQL
  , module Pluto.Migration.Store.FileSystem
  , module Pluto.Migration.Types
  ) where

import           Control.Monad                      (forM_, mapM)
import qualified Data.Set                           as S
import qualified Data.Time                          as Time

import           Pluto.Migration.Backend.PostgreSQL
import           Pluto.Migration.Store.FileSystem
import           Pluto.Migration.Types

formatTimePattern :: String
formatTimePattern = "%Y-%m-%d-%H%M%S"

-- | Given a 'Backend', ensure that the backend is initialized.
ensureInitializeBackend :: MigrationBackend -> IO ()
ensureInitializeBackend backend = do
  createDatabaseIfNeeded backend
  s <- initialized backend
  if s
    then return ()
    else getInitMigration backend >>= installMigration backend

data State
  = Pending
  | Installed
  | Unknow
  deriving (Show)

-- | Return a list of 'Migration' id for all pending states.
listPendingMigrations :: MigrationBackend -> MigrationStore -> IO [String]
listPendingMigrations backend store = do
  storeMigrations <- S.fromList <$> listMigrations store
  backendMigrations <- S.fromList <$> getMigrations backend
  return $ S.toList $ S.difference storeMigrations backendMigrations

-- | Install 'Migration' in all pending states.
installPendingMigrations :: MigrationBackend -> MigrationStore -> IO ()
installPendingMigrations backend store =
  listPendingMigrations backend store >>= installMigrations backend store

-- | Revert 'Migration' in all installed states.
revertInstalledMigrations :: MigrationBackend -> MigrationStore -> IO ()
revertInstalledMigrations backend store =
  getMigrations backend >>= revertMigrations backend store

-- | Redo 'Migration' in all installed states.
redoInstalledMigrations :: MigrationBackend -> MigrationStore -> IO ()
redoInstalledMigrations backend store = do
  ms <- getMigrations backend
  revertMigrations backend store ms
  installMigrations backend store ms

-- | Generate a 'Migration' to 'MigrationStore'
generateMigration :: MigrationStore -> String -> IO ()
generateMigration store name = do
  time <- Time.getCurrentTime
  let version = Time.formatTime Time.defaultTimeLocale formatTimePattern time
      m = Migration { mId = version ++ "_" ++ name
                    , mApply = "-- Apply SQL"
                    , mRevert = "-- Revert SQL"
                    }
  saveMigration store m
  putStrLn $ "Generated " ++ mId m

installMigrations :: MigrationBackend -> MigrationStore -> [String] -> IO ()
installMigrations backend store = runMigrations store run'
  where
    run' m = do
      putStrLn $ "Install " ++ mId m
      installMigration backend m

revertMigrations :: MigrationBackend -> MigrationStore -> [String] -> IO ()
revertMigrations backend store ms = runMigrations store run' (reverse ms)
  where
    run' m = do
      putStrLn $ "Revert " ++ mId m
      revertMigration backend m

runMigrations ::
     MigrationStore
  -> (Migration -> IO ())
  -> [String]
  -> IO ()
runMigrations store run' ms' = do
  ms <- mapM (lookupMigration store) ms'
  forM_ ms run'
