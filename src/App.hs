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
import           Adapter.PostgreSQL.UserData

import Servant.Auth.Server


import Data.Pool
import Database.PostgreSQL.Simple hiding ((:.))
import qualified Database.Redis as R

run :: IO ()
run = do
  let port = 7000
  let connStr = ""
  pool <- initConnectionPool connStr
  redConn <- R.checkedConnect R.defaultConnectInfo
  initDB connStr
  let settings = setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
        defaultSettings
  -- We *also* need a key to sign the cookies
  myKey <- generateKey
  -- Adding some configurations. 'Cookie' requires, in addition to
  -- CookieSettings, JWTSettings (for signing), so everything is just as before
  let jwtCfg = defaultJWTSettings myKey
      cfg = defaultCookieSettings :. jwtCfg :. EmptyContext
      cookieSettings = defaultCookieSettings {cookieIsSecure = NotSecure, cookieXsrfSetting = Nothing}
  Network.Wai.Handler.Warp.run 7000 $ serveWithContext api cfg (server pool redConn cookieSettings jwtCfg)
  -- Network.Wai.Handler.Warp.run 7000 $ serveWithContext api cfg (server pool redConn defaultCookieSettings jwtCfg)
  -- manager <- newManager defaultManagerSettings
  -- runSettings settings $ mkApp manager cfg jwtCfg pool redConn
    
-- mkApp ::
--      Manager
--   -> CookieSettings
--   -> JWTSettings
--   -> Pool Connection
--   -> R.Connection
--   -> Application
-- mkApp manager cfg jwtCfg conns redConn = cors (const . Just $ corsPolicy) $
--   (serveWithContext api cfg $ (server conns redConn defaultCookieSettings jwtCfg) :<|> forwardServer manager)
--   where

--     -- Need to explictly allow needed extra headers through CORS.
--     corsPolicy = simpleCorsResourcePolicy
--       { corsRequestHeaders = [ "content-type" ]
--       }
    