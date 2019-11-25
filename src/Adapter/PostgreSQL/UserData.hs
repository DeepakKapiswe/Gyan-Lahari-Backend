{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module Adapter.PostgreSQL.UserData where

import Data.ByteString (ByteString)
import Control.Concurrent
import Control.Exception (bracket)
import Control.Monad.IO.Class
import Data.Pool
import Database.PostgreSQL.Simple
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Servant
import Servant.Client
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Data.Aeson
import GHC.Generics

type DBConnectionString = ByteString

newtype Message = MkMessage { m :: String }
  deriving (Show, Eq, Generic )

instance FromJSON Message
instance ToJSON Message

type API = ReqBody '[JSON] Message :> Post '[JSON] NoContent
      :<|> Get '[JSON] [Message]
      :<|> "user" :> Get '[JSON] String

api :: Proxy API
api = Proxy

initDB :: DBConnectionString -> IO ()
initDB connstr = bracket (connectPostgreSQL connstr) close $ \conn -> do
  execute_ conn
    "CREATE TABLE IF NOT EXISTS messages (msg text not null)"
  return ()

server :: Pool Connection -> Server API
server conns = postMessage :<|> getMessages :<|> somedata
  where postMessage :: Message -> Handler NoContent
        postMessage msg = do
          liftIO . withResource conns $ \conn ->
            execute conn
                    "INSERT INTO messages VALUES (?)"
                    (Only (m msg))
          return NoContent
        getMessages :: Handler [Message]
        getMessages = fmap (map (MkMessage .fromOnly)) . liftIO $
         withResource conns $ \conn ->
            query_ conn "SELECT msg FROM messages" :: IO [Only String]
        somedata :: Handler String
        somedata = return "Jai Guru Maa from new API"

runApp :: Pool Connection -> IO ()
runApp conns = run 7000 . simpleCors $ (serve api $ server conns)

initConnectionPool :: DBConnectionString -> IO (Pool Connection)
initConnectionPool connStr =
  createPool (connectPostgreSQL connStr)
             close
             2 -- stripes
             60 -- unused connections are kept open for a minute
             10 -- max. 10 connections open per stripe

-- postMsg :: Message -> ClientM NoContent
-- getMsgs :: ClientM [Message]
-- postMsg :<|> getMsgs = client api

main :: IO ()
main = do
  -- you could read this from some configuration file,
  -- environment variable or somewhere else instead.
  -- you will need to either change this connection string OR
  -- set some environment variables (see
  -- https://www.postgresql.org/docs/9.5/static/libpq-envars.html)
  -- to point to a running PostgreSQL server for this example to work.
  let connStr = ""
  pool <- initConnectionPool connStr
  initDB connStr
  runApp pool
  -- bracket (runApp pool) killThread (const (return ()))
  -- mgr <- newManager defaultManagerSettings
  -- bracket (forkIO $ runApp pool) killThread $ \_ -> do
  --   ms <- flip runClientM (mkClientEnv mgr (BaseUrl Http "localhost" 8080 "")) $ do
  --     postMsg "hello"
  --     postMsg "world"
  --     getMsgs
  --   print ms


