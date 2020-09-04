{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Adapter.HTTP.Servers.Distributor 
  (pDistributorServer)
where

import Control.Concurrent
import Control.Exception (bracket)
import Control.Monad.IO.Class
import Control.Monad
import Data.Pool
import Database.PostgreSQL.Simple
import Servant
import Data.Maybe

import qualified Data.UUID as U
import Data.UUID.V4
-- import qualified Database.Redis as R
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL

import Servant.Auth.Server

import Adapter.HTTP.Api
import Types


pDistributorServer ::
  Pool Connection
  -> AuthResult (AllowedUserRoles '[UDistributor])
  -> Server DistributorAPI
pDistributorServer pConns (Authenticated (AllowedUser usr)) = distributorServer pConns usr
pDistributorServer _ x = throwAll err401 { errBody = BL.pack $ show x }
 



distributorServer :: Pool Connection -> User -> Server DistributorAPI
distributorServer conns usr = 
  getDistributor

  where
    getDistributor :: Handler [Distributor]
    getDistributor = liftIO $
        withResource conns $ \conn ->
          query conn "SELECT  \
            \ distId,    \
            \ distName,  \
            \ distAdd,   \
            \ distCity,  \
            \ distPhone  \
            \ FROM input_static_distributors \
            \ WHERE   \
              \ distId  = ? "
            [uId usr]
    