{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Adapter.HTTP.Servers.AuthHandler 
  (authHandler)
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


authHandler
  :: Pool Connection
  -> CookieSettings
  -> JWTSettings
  -> Server UnProtectedAPI
authHandler conns cookieSettings jwtSettings = checkCreds
  where
    checkCreds 
      :: UserAuth
      -> Handler (Headers '[ Header "Set-Cookie" SetCookie
                           , Header "Set-Cookie" SetCookie]
                            User)
    checkCreds usr@(UserAuth name pass) = do
      (uName :: [Only String]) <- liftIO $ withResource conns $ \conn ->
                  query conn "SELECT \
                    \ userId         \
                    \ FROM userlogin \
                      \ WHERE \
                     \ userId = ? \
                      \ AND \
                     \ userpassword = ? \
                      \ LIMIT 1"
                    [name, pass]
      
      let userRole | length uName == 0 = UGuest
                   | otherwise =  case length name of
                        11 -> UAdmin
                        2  -> UApprover
                        7  -> UManager
                        4  -> UDistributor
                        _  -> USubscriber

    
      mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings (UAL (User (Just name) userRole))
      case mApplyCookies of
        Nothing           ->
          throwError err401
        Just applyCookies ->
          return $ applyCookies (User (Just "AEDX") userRole)
