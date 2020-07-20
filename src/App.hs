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
  let tls = tlsSettings "./tlsSettings/server.crt" "./tlsSettings/server.key"
  pool <- initConnectionPool connStr
  redConn <- R.checkedConnect R.defaultConnectInfo
  initDB connStr
  let settings = setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
        defaultSettings
  myKey <- generateKey
  let jwtCfg = defaultJWTSettings myKey
      ctx :: Context '[ CookieSettings, JWTSettings ]
      ctx = cookieSettings :. jwtCfg :. EmptyContext
      cookieSettings = defaultCookieSettings {
        cookieXsrfSetting = Nothing,
        cookieSameSite = AnySite }
  runTLS tls (setPort port defaultSettings) .
    -- allowCsrf .
    corsified $
    serveWithContext api ctx (server pool redConn cookieSettings jwtCfg)
    where
    
      -- | @x-csrf-token@ allowance.
      -- The following header will be set: @Access-Control-Allow-Headers: x-csrf-token@.
      allowCsrf :: Middleware
      allowCsrf = addHeaders [("Access-Control-Allow-Headers", "Authorization, Content-Type, X-XSRF-TOKEN")]
      -- | CORS middleware configured with 'appCorsResourcePolicy'.
      corsified :: Middleware
      corsified = cors (const $ Just appCorsResourcePolicy)
      
      -- | Cors resource policy to be used with 'corsified' middleware.
      appCorsResourcePolicy :: CorsResourcePolicy
      appCorsResourcePolicy = CorsResourcePolicy {
          corsOrigins        = Just ([ "http://localhost:3000" ], True)
        , corsMethods        = ["OPTIONS", "GET", "PUT", "POST"]
        , corsRequestHeaders = ["Authorization", "Content-Type","X-XSRF-TOKEN"]
        , corsExposedHeaders = Just ["Authorization", "Content-Type","X-XSRF-TOKEN"]
        , corsMaxAge         = Nothing
        , corsVaryOrigin     = False
        , corsRequireOrigin  = False
        , corsIgnoreFailures = False
      }