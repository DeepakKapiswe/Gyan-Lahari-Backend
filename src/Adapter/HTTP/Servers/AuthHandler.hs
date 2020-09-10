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
authHandler conns cookieSettings jwtSettings = 
       checkCreds
  :<|> distCheckCreds
  where
    checkCreds 
      :: UserAuth
      -> Handler (Headers '[ Header "Set-Cookie" SetCookie
                           , Header "Set-Cookie" SetCookie]
                            User)
    checkCreds usr@(UserAuth name pass muserType) = do
      let (queryStr, queryParams) =
            case muserType of 
              Just "Subscriber"  -> ("SELECT \
                                    \ subId         \
                                    \ FROM input_dynamic_subscribers \
                                      \ WHERE \
                                     \ subPhone = ? \
                                      \ LIMIT 1", [name])
              Just "Distributor" -> ("SELECT \
                                    \ distId         \
                                    \ FROM input_static_distributors \
                                      \ WHERE \
                                     \ distPhone = ? \
                                      \ LIMIT 1", [name])
              -- Anything for now, later it could have more details
              _                  -> ("SELECT \            
                                    \ userRole  \
                                    \ FROM userlogin \
                                      \ WHERE \
                                     \ userId = ? \
                                      \ AND \
                                     \ userpassword = ? \
                                      \ LIMIT 1", [name, pass])

      (uName :: [Only String]) <- liftIO $ withResource conns $ \conn ->
                  query conn queryStr queryParams
      let dbReturnVal = fromOnly . head $ uName
      let (userRole, usrId) | length uName == 0 = (UGuest, "")
                            | otherwise =  case muserType of
                                 Just  "Subscriber"  -> (USubscriber, dbReturnVal)
                                 Just  "Distributor" -> (UDistributor, dbReturnVal)
                                 _ -> (read dbReturnVal, "")
          
      mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings (AllowedUser (User (Just usrId) userRole))
      case mApplyCookies of
        Nothing           ->
          throwError err401
        Just applyCookies ->
          return $ applyCookies (User (Just usrId) userRole)

    distCheckCreds
      :: UserAuth
      -> Handler (Headers '[ Header "Set-Cookie" SetCookie
                           , Header "Set-Cookie" SetCookie]
                            Distributor)
    distCheckCreds usr@(UserAuth name pass muserType) =
      case muserType of 
        Just "Distributor" -> do
          -- (uName :: [Only String]) <- liftIO $ withResource conns $ \conn ->
          --           query conn 
          --             "SELECT  \
          --               \ distId \
          --             \ FROM input_static_distributors \
          --               \ WHERE \
          --             \ distPhone = ? \
          --             \ LIMIT 1"
          --             [name]
          res <- liftIO $ withResource conns $ \conn ->
            query conn "SELECT  \
              \ distId,    \
              \ distName,  \
              \ distAdd,   \
              \ distCity,  \
              \ distPhone  \
              \ FROM input_static_distributors \
              \ WHERE   \
                \ distPhone  = ? "
              [name]

          let dbReturnVal = head $ res
          let (userRole, usrId) | length res == 0 = (UGuest, "")
                                | otherwise = (UDistributor, fromJust $ distId dbReturnVal)
              
          mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings (AllowedUser (User (Just usrId) userRole))
          case mApplyCookies of
            Nothing           ->
              throwError err401
            Just applyCookies ->
              return $ applyCookies dbReturnVal
        
        _                  -> throwError err401