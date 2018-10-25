{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}

module Pluto.API.Group where

import           Servant

import qualified Feature.Group.Database as Db
import           Feature.Group.Types

import           Pluto.Internal.Types

type API = Get '[JSON] [Group]
           :<|> ReqBody '[JSON] NewGroup :> Post '[JSON] Group
           :<|> Capture "groupId" GroupId :> Get '[JSON] Group
           :<|> Capture "groupId" GroupId :> ReqBody '[JSON] UpdateGroup :> Put '[JSON] Group
           :<|> Capture "groupId" GroupId :> Delete '[JSON] NoContent

handler :: ServerT API (PlutoAppT IO)
handler = findAll :<|> create :<|> findById :<|> update :<|> deleteById
  where
    findAll = Db.findAll
    create = Db.create
    update id' group = maybe404 =<< Db.update id' group
    findById id' = maybe404 =<< Db.findById id'
    deleteById id' = deleteById id' >> return NoContent

maybe404 :: (MonadError PlutoErr m, Monad m) => Maybe a -> m a
maybe404 (Just a) = return a
maybe404 _        = throwError err404
