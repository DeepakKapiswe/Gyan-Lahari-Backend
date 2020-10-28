{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Adapter.HTTP.Servers.Approver
    (pApproverServer)
where

import Control.Concurrent
import Control.Exception (bracket)
import Control.Monad.IO.Class
import Control.Monad
import Data.Pool
import Database.PostgreSQL.Simple
import Servant
import qualified Data.ByteString.Lazy.Char8 as BL

import Servant.Auth.Server

import Adapter.HTTP.Api
import Types
import Adapter.HTTP.Servers.AuthHandler


pApproverServer::
     Pool Connection
  -> AuthResult (AllowedUserRoles '[UApprover, UAdmin])
  -> Server ApproverAPI
pApproverServer conns (Authenticated (AllowedUser usr)) = approverServer conns usr
pApproverServer _ x = throwAll err401 { errBody = BL.pack $ show x }


approverServer :: Pool Connection -> User -> Server ApproverAPI
approverServer conns usr =
  postSubscriber           :<|> 
  updateSubscriber         :<|>
  postDistributor          :<|>
  updateDistributor        :<|>
  processSubscriberApplication Approved :<|>
  processSubscriberApplication Rejected 
  where
    postSubscriber :: Subscriber -> Handler Subscriber
    postSubscriber subscriber = do
      res <- liftIO . withResource conns $ \conn -> 
       query conn
        "INSERT INTO input_dynamic_subscribers( \
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
        \ subEndVol     ) VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?) \
        \ RETURNING \
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
        \  subEndVol     "

        [ pure "AAAA"
        , show <$> subStartVol subscriber
        , show <$> subSubscriptionType subscriber
        , show <$> subSlipNum subscriber
        , subName    subscriber
        , subAbout   subscriber
        , subAdd1    subscriber
        , subAdd2    subscriber
        , subPost    subscriber
        , subCity    subscriber
        , subState   subscriber
        , subPincode subscriber
        , subPhone   subscriber
        , subRemark  subscriber
        , subDistId  subscriber
        , show <$> subEndVol  subscriber ]
      return $ head res
    
    updateSubscriber :: Subscriber -> Handler String
    updateSubscriber subscriber = do
      liftIO . withResource conns $ \conn ->
        execute conn
          "UPDATE input_dynamic_subscribers \
            \ SET \
              \ subStartVol = ?,         \
              \ subSubscriptionType = ?, \
              \ subSlipNum = ?,   \
              \ subName = ?,      \
              \ subAbout = ?,     \
              \ subAdd1 = ?,      \
              \ subAdd2 = ?,      \
              \ subPost = ?,      \
              \ subCity = ?,      \
              \ subState = ?,     \
              \ subPincode = ?,   \
              \ subPhone = ?,     \
              \ subRemark = ?,    \
              \ subDistId = ?,    \
              \ subEndVol = ?     \
            \ WHERE   \
              \ subId = ? "
        [ show <$> subStartVol subscriber
        , show <$> subSubscriptionType subscriber
        , show <$> subSlipNum subscriber
        , subName    subscriber
        , subAbout   subscriber
        , subAdd1    subscriber
        , subAdd2    subscriber
        , subPost    subscriber
        , subCity    subscriber
        , subState   subscriber
        , subPincode subscriber
        , subPhone   subscriber
        , subRemark  subscriber
        , subDistId  subscriber 
        , show <$> subEndVol  subscriber
        , subId      subscriber ]
      return "Data Updated on Database"
    
    postDistributor :: Distributor -> Handler Distributor
    postDistributor distributor = do
      res <- liftIO . withResource conns $ \conn ->
        query conn
          "INSERT INTO input_static_distributors( \    
           \ distId,    \
           \ distName,  \
           \ distAdd,   \
           \ distCity,  \
           \ distPhone  \
           \ ) VALUES (?,?,?,?,?) \
           \ RETURNING \
           \ distId,    \
           \ distName,  \
           \ distAdd,   \
           \ distCity,  \
           \ distPhone  "

        [ distId distributor
        , distName distributor
        , distAdd distributor
        , distCity distributor
        , distPhone distributor ]
      return $ head res 
         
    updateDistributor :: Distributor -> Handler String
    updateDistributor distributor = do
      liftIO . withResource conns $ \conn ->
        execute conn
          "UPDATE input_static_distributors \
            \ SET \
          \ distName  = ?,  \
          \ distAdd   = ?,  \
          \ distCity  = ?,  \
          \ distPhone = ?   \
          \ WHERE   \
            \ distId  = ? "
        [ distName distributor
        , distAdd distributor
        , distCity distributor
        , distPhone distributor
        , distId distributor ]
      return "Data Tried to add on Database"
    
    processSubscriberApplication :: ApplicationState -> ApprovalRequest -> Handler SubscriberApplication
    processSubscriberApplication appState appReq = do
      liftIO . withResource conns $ \conn ->
        execute conn
          "UPDATE input_dynamic_subscriber_applications \
            \ SET \
              \ appStatus = ?, \
              \ processedBy = ? \
            \ WHERE \
              \ (appStatus = \'Pending\') \
                \ AND \
              \ (subAppId = ?)"
          ( show appState
          , arProcessedBy appReq
          , show <$> arApplicationId appReq)
      getSubscriberApplication $ arApplicationId appReq

    -- Extra care should be taken
    -- This function should not be called with an appId not present in DB
    getSubscriberApplication :: Maybe Int -> Handler SubscriberApplication
    getSubscriberApplication appId = liftIO $
      withResource conns $ \conn -> do
        res <- query conn "SELECT \
              \  subAppId,    \
              \  appType,     \
              \  appStatus,   \
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
              \  subEndVol \
              \ FROM input_dynamic_subscriber_applications \
              \  WHERE \
              \  subAppId = ?"
            [show <$> appId]
        return $ head res    