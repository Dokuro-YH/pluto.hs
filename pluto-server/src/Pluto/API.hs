{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Pluto.API where

import           Network.HTTP.Types.Status (status404)
import           Network.Wai               (responseLBS)
import           Servant

import qualified Pluto.API.Group           as Group
import           Pluto.Internal.Types

type PlutoAPI = "groups" :> Group.API
           :<|> Raw

api :: Proxy PlutoAPI
api = Proxy

handler :: ServerT PlutoAPI (PlutoAppT IO)
handler = Group.handler :<|> pure notFound
  where
    notFound _ respond = respond $ responseLBS status404 [] mempty
