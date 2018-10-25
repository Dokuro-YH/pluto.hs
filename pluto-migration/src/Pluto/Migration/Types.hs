module Pluto.Migration.Types where

data Migration = Migration
  { mId     :: String
  , mApply  :: String
  , mRevert :: String
  } deriving (Show, Eq)

-- | The type of 'Migration' storage facilities.
data MigrationStore = MigrationStore
  { initStore       :: IO () -- ^ Creates migrations directory.
  , listMigrations  :: IO [String] -- ^ Returns a list of all migration IDs.
  , lookupMigration :: String -> IO Migration -- ^ Get a migration from the store.
  , saveMigration   :: Migration -> IO () -- ^ Generate a migration to the store.
  }

-- | A 'MigrationBackend' represents a database engine backend such as MySQL/PostgreSQL/SQLite.
data MigrationBackend = MigrationBackend
  { initialized            :: IO Bool -- ^ Returns whether the backend has been initialized.
  , createDatabaseIfNeeded :: IO () -- ^ Creates new database if no exist.
  , getInitMigration       :: IO Migration -- ^ The migration to initialize database.
  , getMigrations          :: IO [String] -- ^ Returns a list of installed migration IDs from the backend.
  , installMigration       :: Migration -> IO () -- ^ Install the specified migration on the backend.
  , revertMigration        :: Migration -> IO () -- ^ Revert the specified migration from the backend.
  }
