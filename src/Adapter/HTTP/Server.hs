{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}


module Adapter.HTTP.Server where

import Control.Concurrent
import Control.Exception (bracket)
import Control.Monad.IO.Class
import Data.Pool
import Database.PostgreSQL.Simple
import Servant

import Adapter.HTTP.Api
import Types


server :: Pool Connection -> Server API
server conns =
  postMessage :<|> 
  getMessages :<|> 
  somedata     
  where
    postMessage :: CustAddress -> Handler String
    postMessage cAdd = do
      liftIO . withResource conns $ \conn ->
        execute conn
                    "INSERT INTO input_dynamic_customerDetails(    \    
                     \ custSaluation, \
                     \ custFname,     \
                     \ custMname,     \
                     \ custLname,     \
                     \ custAbout,     \
                     \ custAdd1,      \
                     \ custAdd2,      \
                     \ custPost,      \
                     \ custCity,      \
                     \ custState,     \
                     \ custPincode,   \
                     \ custPhone) VALUES (?,?,?,?,?,?,?,?,?,?,?,?)"
                    [ custSaluation cAdd
                    , custFname  cAdd
                    , custMname  cAdd
                    , custLname  cAdd
                    , custAbout  cAdd
                    , custAdd1   cAdd
                    , custAdd2   cAdd
                    , custPost   cAdd
                    , custCity   cAdd
                    , custState  cAdd
                    , custPincode cAdd
                    , custPhone  cAdd ]
      return "Data Tried to add on Database"
            
    getMessages :: Handler [Message]
    getMessages = fmap (map (MkMessage .fromOnly)) . liftIO $
      withResource conns $ \conn ->
      query_ conn "SELECT custFname FROM input_dynamic_customerDetails" :: IO [Only String]
            
    somedata :: Handler String
    somedata = return "Jai Guru Maa from new API"
      

