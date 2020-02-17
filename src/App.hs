{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module App where

import           Data.Aeson
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import           Network.HTTP.ReverseProxy
import           Servant
import           System.IO

import           Network.Wai.Middleware.Cors

import           Adapter.HTTP.Api
import           Adapter.HTTP.Server
import           Adapter.HTTP.ProxyServer
import           Adapter.HTTP.PostgreSQL.UserData


import Data.Pool
import Database.PostgreSQL.Simple

run :: IO ()
run = do
  let port = 7000
  let connStr = ""
  pool <- initConnectionPool connStr
  initDB connStr
  let settings = setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
        defaultSettings
  manager <- newManager defaultManagerSettings
  runSettings settings $ mkApp manager pool
    
mkApp :: Manager -> Pool Connection -> Application
mkApp manager conns = cors (const . Just $ corsPolicy) $
  (serve api $ (server conns) :<|> forwardServer manager)
  where

    -- Need to explictly allow needed extra headers through CORS.
    corsPolicy = simpleCorsResourcePolicy
      { corsRequestHeaders = [ "content-type" ]
      }
    
-- runApp :: Pool Connection -> IO ()
-- runApp conns = do
--   run 7000 $ app manager conns
