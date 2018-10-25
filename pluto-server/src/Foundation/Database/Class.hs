module Foundation.Database.Class
  ( MonadDb(..)
  ) where

import           Control.Monad.IO.Class             (MonadIO (..))
import           Database.PostgreSQL.Simple         (Query)
import           Database.PostgreSQL.Simple.FromRow (FromRow (..))
import           Database.PostgreSQL.Simple.ToRow   (ToRow (..))

class MonadIO m => MonadDb m where
  runQuery :: (ToRow q, FromRow a) => Query -> q -> m [a]
  runQuery_ :: FromRow a => Query -> m [a]
  runUpdate :: ToRow q => Query -> q -> m Int
  runReturning :: (ToRow q, FromRow a) => Query -> q -> m (Maybe a)
  runReturning' :: (ToRow q, FromRow a) => Query -> q -> m a
  runReturningMany :: (ToRow q, FromRow a) => Query -> [q] -> m [a]
