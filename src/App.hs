{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module App where

import           Data.Aeson
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Handler.WarpTLS
import           Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import           Network.HTTP.ReverseProxy
import           Servant
import           System.IO

import           Network.Wai.Middleware.Cors
import           Network.Wai.Middleware.AddHeaders (addHeaders)
import           Network.Wai.Middleware.Servant.Options

import           Adapter.HTTP.Api
import           Adapter.HTTP.Server
import           Adapter.HTTP.ProxyServer
import           Adapter.PostgreSQL.UserData

import           Servant.Auth.Server


import Data.Pool
import Database.PostgreSQL.Simple hiding ((:.))
import qualified Database.Redis as R

runApp :: IO ()
runApp = do
  let port = 7000
  let connStr = ""

  pool <- initConnectionPool connStr
  redConn <- R.checkedConnect R.defaultConnectInfo
  initDB connStr

  myKey <- generateKey
  let jwtCfg = defaultJWTSettings myKey
      ctx :: Context '[ CookieSettings, JWTSettings ]
      ctx = cookieSettings :. jwtCfg :. EmptyContext
      cookieSettings = defaultCookieSettings
        { cookieSameSite = SameSiteStrict}
  run 7000 $
    serveWithContext api ctx (server pool redConn cookieSettings jwtCfg)
    