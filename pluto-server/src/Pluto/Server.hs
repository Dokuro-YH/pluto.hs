{-# LANGUAGE FlexibleContexts #-}

module Pluto.Server
  ( server
  , defaultSettings
  , PlutoSettings(..)
  ) where

import           Network.Wai              (Application)
import           Network.Wai.Handler.Warp (run)
import           Servant                  (Server, hoistServer, serve)

import           Pluto.API
import           Pluto.Internal.Types

app :: PlutoEnv -> Server PlutoAPI
app env = hoistServer api nt handler
  where
    nt action = liftIO (runPlutoAppT env action) >>= either throwError pure

mkApp :: PlutoEnv -> Application
mkApp env = serve api $ app env

server :: PlutoSettings -> IO ()
server settings = do
  env <- mkEnv settings
  putStrLn $ "Pluto-Server started on http://127.0.0.1:" ++ show port ++ " (ctrl+c quit)"
  run port $ mkApp env
  where
    port = sPort settings
