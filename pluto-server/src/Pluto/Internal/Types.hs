{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Pluto.Internal.Types
  ( PlutoAppT
  , PlutoErr
  , runPlutoAppT
  , PlutoSettings(..)
  , PlutoEnv(..)
  , defaultSettings
  , mkEnv
  , getPool
  -- * IO Exception handler
  , try
  , catch
  , finally
  , handle
  , bracket
  , bracketOnError
  -- * Re-exports
  , asks
  , MonadReader(..)
  , MonadError(..)
  , MonadIO(..)
  ) where

import           Control.Exception           (SomeException (..))
import           Control.Exception.Lifted    (bracket, bracketOnError, catch,
                                              finally, handle, try)
import           Control.Monad.Base          (MonadBase (..), liftBaseDefault)
import           Control.Monad.Except        (ExceptT, MonadError (..),
                                              runExceptT)
import           Control.Monad.IO.Class      (MonadIO (..))
import           Control.Monad.Reader        (MonadReader (..), ReaderT, asks,
                                              runReaderT)
import           Control.Monad.Trans.Class   (MonadTrans (..))
import           Control.Monad.Trans.Control (ComposeSt, MonadBaseControl (..),
                                              MonadTransControl (..),
                                              defaultLiftBaseWith,
                                              defaultRestoreM)
import           Data.ByteString.Char8       (pack)
import           Data.Maybe                  (listToMaybe)
import           Data.Pool                   (Pool, createPool, withResource)
import           Database.PostgreSQL.Simple  (Connection, close,
                                              connectPostgreSQL, execute, query,
                                              query_, returning)

import           Foundation.Database.Class   (MonadDb (..))
import           Servant                     (ServantErr (..), err500)

data PlutoSettings = PlutoSettings
  { sPort        :: Int
  , sDatabaseUrl :: String
  } deriving (Show)

data PlutoEnv = PlutoEnv
  { envPool :: Pool Connection
  } deriving (Show)

type PlutoErr = ServantErr

newtype PlutoAppT m a
  = PlutoAppT { unPlutoAppT :: ExceptT PlutoErr (ReaderT PlutoEnv m) a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadError PlutoErr
             , MonadReader PlutoEnv
             )

instance (MonadBase b m) => MonadBase b (PlutoAppT m) where
  liftBase = liftBaseDefault

instance MonadTrans PlutoAppT where
  lift = PlutoAppT . lift . lift

instance MonadBaseControl b m => MonadBaseControl b (PlutoAppT m) where
  type StM (PlutoAppT m) a = ComposeSt PlutoAppT m a
  liftBaseWith             = defaultLiftBaseWith
  restoreM                 = defaultRestoreM

instance MonadTransControl PlutoAppT where
  type StT PlutoAppT a = StT (ExceptT PlutoErr) (StT (ReaderT PlutoEnv) a)
  liftWith f = PlutoAppT $ liftWith $ \run ->
                                    liftWith $ \run' ->
                                                f (run' . run . unPlutoAppT)
  restoreT = PlutoAppT . restoreT . restoreT

defaultSettings :: PlutoSettings
defaultSettings = PlutoSettings 8080 "postgres://localhost:5432/pluto"

mkEnv :: PlutoSettings -> IO PlutoEnv
mkEnv PlutoSettings {..} = PlutoEnv <$> mkPool sDatabaseUrl 8

mkPool :: String -> Int -> IO (Pool Connection)
mkPool url = createPool open close 1 30
  where
    open = connectPostgreSQL $ pack url

getPool :: MonadReader PlutoEnv m => m (Pool Connection)
getPool = asks envPool

runPlutoAppT :: Monad m => PlutoEnv -> PlutoAppT m a -> m (Either PlutoErr a)
runPlutoAppT env (PlutoAppT m) = runReaderT (runExceptT m) env

instance (MonadBaseControl IO m, MonadIO m) => MonadDb (PlutoAppT m) where
  runQuery sql q = getPool >>= runDb (\conn -> query conn sql q) err500
  runQuery_ sql = getPool >>= runDb (`query_` sql) err500
  runUpdate sql q = getPool >>= runDb (\conn -> fromIntegral <$> execute conn sql q) err500
  runReturning sql q = getPool >>= runDb (\conn -> listToMaybe <$> returning conn sql [q]) err500
  runReturning' sql q = getPool >>= runDb (\conn -> head <$> returning conn sql [q]) err500
  runReturningMany sql qs = getPool >>= runDb (\conn -> returning conn sql qs) err500

runDb ::
     (MonadBaseControl IO m, MonadIO m)
  => (Connection -> IO a)
  -> PlutoErr
  -> Pool Connection
  -> PlutoAppT m a
runDb action err pool = catch (liftIO $ withResource pool action) handler
  where
    handler (SomeException e) = do
      liftIO $ print e -- FIXME: use logging
      throwError err
