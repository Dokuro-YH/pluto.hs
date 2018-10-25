{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Pluto.API.Group where

import           Servant

import           Feature.Group.Database
import           Feature.Group.Types

import           Pluto.Internal.Types

type API = Get '[JSON] [Group]
           :<|> ReqBody '[JSON] NewGroup :> Post '[JSON] Group
           :<|> Capture "groupId" GroupId :> Get '[JSON] (Maybe Group)
           :<|> Capture "groupId" GroupId :> ReqBody '[JSON] UpdateGroup :> Put '[JSON] (Maybe Group)
           :<|> Capture "groupId" GroupId :> Delete '[JSON] NoContent

handler :: ServerT API (PlutoAppT IO)
handler = findAll :<|> create :<|> findById :<|> update :<|> noContent' deleteById

noContent' :: Monad m => (a -> m b) -> a -> m NoContent
noContent' f a = f a >> return NoContent
