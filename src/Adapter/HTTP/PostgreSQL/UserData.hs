{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module Adapter.HTTP.PostgreSQL.UserData where

import Data.ByteString as BS (ByteString, readFile)
import Control.Concurrent
import Control.Exception (bracket)
import Data.Pool
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types


type DBConnectionString = ByteString

initDB :: DBConnectionString -> IO ()
initDB connstr = 
  return ()


initConnectionPool :: DBConnectionString -> IO (Pool Connection)
initConnectionPool connStr =
  createPool (connectPostgreSQL connStr)
             close
             2 -- stripes
             60 -- unused connections are kept open for a minute
             10 -- max. 10 connections open per stripe
