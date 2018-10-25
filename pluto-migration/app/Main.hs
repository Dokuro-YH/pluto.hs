{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards    #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

module Main where

import           Configuration.Dotenv       (load, loadFile)
import           Configuration.Dotenv.Types (defaultConfig)
import           Control.Exception          (SomeException (..), catch)
import           Data.Version               (showVersion)
import           System.Console.CmdArgs
import           System.Environment         (getArgs, lookupEnv, withArgs)

import           Paths_pluto_migration      (version)
import           Pluto.Migration

data Cmd
  = CmdSetup { cmdDatabaseUrl  :: String
             , cmdMigrationDir :: FilePath }
  | CmdGenerate { cmdDatabaseUrl  :: String
                , cmdMigrationDir :: FilePath
                , cmdGenerateName :: String }
  | CmdPending { cmdDatabaseUrl  :: String
               , cmdMigrationDir :: FilePath }
  | CmdInstall { cmdDatabaseUrl :: String
           , cmdMigrationDir    :: FilePath }
  | CmdRevert { cmdDatabaseUrl  :: String
              , cmdMigrationDir :: FilePath }
  | CmdRedo { cmdDatabaseUrl  :: String
            , cmdMigrationDir :: FilePath }
  deriving (Show, Data, Typeable)

mode :: Mode (CmdArgs Cmd)
mode =
  cmdArgsMode $
  modes
    [ CmdSetup
        { cmdDatabaseUrl = def &= typ "DATABASE_URL" &= name "d" &= name "database_url" &= explicit
        , cmdMigrationDir = def &= typDir &= name "dir" &= explicit
        } &= explicit &= name "setup" &= setupHelp
    , CmdGenerate
        { cmdGenerateName = def &= argPos 0 &= typ "NAME"
        } &= explicit &= name "generate" &= generateHelp
    , CmdPending {} &= explicit &= name "pending" &= help "Returns true if there are any pending migrations."
    , CmdInstall {} &= explicit &= name "install" &= help "Install all pending migrations."
    , CmdRevert {} &= explicit &= name "revert" &= help "Revert all installed migrations."
    , CmdRedo {} &= explicit &= name "redo" &= help "Redo all installed migrations."
    ] &= program "pluto-migration"
    &= summary ("pluto-migration " ++ showVersion version)
    &= helpArg [explicit, name "h", name "help"]
  where
    setupHelp = help "Creates the migrations dirctory, creates the database specified in your DATABASE_URL, and then runs any existing migrations."
    generateHelp = help "Generate a new migration with the given name, and the current timestamp as the version."

main :: IO ()
main = do
  dotenv
  getArgs >>= getCmd >>= runCmd

getCmd :: [String] -> IO Cmd
getCmd []    = withArgs ["--help"] $ cmdArgsRun mode
getCmd args' = withArgs args' $ automatic =<< cmdArgsRun mode

automatic :: Cmd -> IO Cmd
automatic cmd = url =<< path cmd
  where
    path cmd' = return $ if null $ cmdMigrationDir cmd' then cmd' {cmdMigrationDir = "./migrations"} else cmd'
    url cmd' = do
      envUrl <- lookupEnv "DATABASE_URL"
      case envUrl of
        Just url' -> return $ cmd' {cmdDatabaseUrl = url'}
        Nothing -> if null $ cmdDatabaseUrl cmd'
                   then fail "DATABASE_URL no set"
                   else return cmd'

runCmd :: Cmd -> IO ()
runCmd cmd =
  case cmd of
    CmdSetup {..} -> initStore store >> ensureInitializeBackend backend >> installPendingMigrations backend store
    CmdGenerate {..} -> generateMigration store cmdGenerateName
    CmdPending {..} -> pendingPrint =<< listPendingMigrations backend store
    CmdInstall {..} -> installPendingMigrations backend store
    CmdRevert {..} -> revertInstalledMigrations backend store
    CmdRedo {..} -> redoInstalledMigrations backend store
  where
    store = fsStore $ cmdMigrationDir cmd
    backend = pgBackend $ cmdDatabaseUrl cmd

pendingPrint :: [String] -> IO ()
pendingPrint [] = putStrLn "All installed"
pendingPrint xs = mapM_ putStrLn xs

dotenv :: IO ()
dotenv = (loadFile defaultConfig >>= load False) `catch` (\(SomeException _) -> return ())
