{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module Adapter.HTTP.Server where

import Control.Concurrent
import Control.Exception (bracket)
import Control.Monad.IO.Class
import Data.Pool
import Database.PostgreSQL.Simple
import Servant

import Adapter.HTTP.Api


server :: Pool Connection -> Server API
server conns =
  postMessage :<|> 
  getMessages :<|> 
  somedata     
  where
    postMessage :: Message -> Handler Message -- NoContent
    postMessage msg = do
      liftIO . withResource conns $ \conn ->
        execute conn
                    "INSERT INTO messages VALUES (?)"
                    (Only $ m msg)
      return msg -- NoContent
            
    getMessages :: Handler [Message]
    getMessages = fmap (map (MkMessage .fromOnly)) . liftIO $
      withResource conns $ \conn ->
      query_ conn "SELECT msg FROM messages" :: IO [Only String]
            
    somedata :: Handler String
    somedata = return "Jai Guru Maa from new API"
      

