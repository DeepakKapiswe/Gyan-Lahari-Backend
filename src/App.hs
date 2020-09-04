{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module App where

import           Data.Aeson
import           GHC.Generics
import           Control.Exception
import qualified Data.ByteString.Char8 as B
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import           Network.HTTP.ReverseProxy
import           Servant
import           System.IO
import           System.Environment (getArgs)

import           Network.Wai.Middleware.Cors
import           Network.Wai.Middleware.AddHeaders (addHeaders)

import           Adapter.HTTP.Api
import           Adapter.HTTP.Server
import           Adapter.PostgreSQL.UserData

import           Servant.Auth.Server


import Data.Pool
import Database.PostgreSQL.Simple hiding ((:.))
import qualified Database.Redis as R

runApp :: IO ()
runApp = do
  f <- getArgs
  let port = 7000
  connStr <- B.pack <$> case f of
        [configFilePath] -> do
          contents <- safeLoadFile configFilePath
          case contents of
            Left _ -> pure ""
            Right connInfo -> pure connInfo
        _ -> pure ""

  pool <- initConnectionPool connStr
  -- redConn <- R.checkedConnect R.defaultConnectInfo

  myKey <- generateKey
  let jwtCfg = defaultJWTSettings myKey
      ctx :: Context '[ CookieSettings, JWTSettings ]
      ctx = cookieSettings :. jwtCfg :. EmptyContext
      cookieSettings = defaultCookieSettings
        { cookieSameSite = SameSiteStrict}
  run 7000 $
    serveWithContext api ctx (server pool cookieSettings jwtCfg)

-- | Takes a FilePath and returns either its contents if file is present
-- or an IOException if not
safeLoadFile :: FilePath -> IO (Either IOException String)
safeLoadFile f = (Right <$> readFile f) `catch` (\ e -> pure (Left e) )