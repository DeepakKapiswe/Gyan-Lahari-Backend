{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Adapter.HTTP.Servers.Subscriber 
  (pSubscriberServer)
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
import Adapter.HTTP.Handlers.NewApplication (newApplication)

pSubscriberServer ::
  Pool Connection
  -> AuthResult (AllowedUserRoles '[USubscriber])
  -> Server SubscriberAPI
pSubscriberServer pConns (Authenticated (AllowedUser usr)) = subscriberServer pConns usr
pSubscriberServer _ x = throwAll err401 { errBody = BL.pack $ show x }
 



subscriberServer :: Pool Connection -> User -> Server SubscriberAPI
subscriberServer conns usr = 
       getSubscriber
  :<|> getAllSubscriberApplications
  :<|> editDetails
       

  where
    getSubscriber :: Handler [Subscriber]
    getSubscriber = 
      liftIO $ withResource conns $ \conn ->
        query conn "SELECT     \
        \ subId,               \
        \ subStartVol,         \
        \ subSubscriptionType, \
        \ subSlipNum,   \
        \ subName,      \
        \ subAbout,     \
        \ subAdd1,      \
        \ subAdd2,      \
        \ subPost,      \
        \ subCity,      \
        \ subState,     \
        \ subPincode,   \
        \ subPhone,     \
        \ subRemark,    \
        \ subDistId,    \
        \ subEndVol     \
        \ FROM input_dynamic_subscribers \
          \ WHERE \
         \ subId = ? "
        [uId usr]
    
    getAllSubscriberApplications :: Handler [SubscriberApplication]
    getAllSubscriberApplications = liftIO $ 
      withResource conns $ \conn ->
        query conn "SELECT     \
        \  subAppId, \
        \  appType,  \
        \  appStatus, \
        \  processedBy, \
          \  subId,       \
          \  subStartVol, \
          \  subSubscriptionType, \
          \  subSlipNum,   \
          \  subName,      \
          \  subAbout,     \
          \  subAdd1,      \
          \  subAdd2,      \
          \  subPost,      \
          \  subCity,      \
          \  subState,     \
          \  subPincode,   \
          \  subPhone,     \
          \  subRemark,    \
          \  subDistId,    \
          \  subEndVol     \
          \ FROM input_dynamic_subscriber_applications \
          \   WHERE subId = ? \
          \ ORDER BY subAppId DESC"
        [uId usr]
    
    editDetails :: Subscriber -> Handler SubscriberApplication
    editDetails sub = newApplication conns EditDetails sub'
      where
        sub' = sub {subId = uId usr}