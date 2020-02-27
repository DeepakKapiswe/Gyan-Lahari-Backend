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
    postMessage :: Subscriber -> Handler String
    postMessage subscriber = do
      liftIO . withResource conns $ \conn ->
        execute conn
                    "INSERT INTO input_dynamic_subscribers( \    
                     \ subStartVol, \
                     \ subSubscriptionType, \
                     \ subSlipNum, \
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
                     \ subDistId     ) VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?)"
        [ subStartVol subscriber
        , subSubscriptionType subscriber
        , subSlipNum subscriber
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
        , subDistId  subscriber ]
      return "Data Tried to add on Database"
                     
    getMessages :: Handler [Subscriber]
    getMessages = liftIO $ 
      withResource conns $ \conn ->
        query_ conn "SELECT  \
          \ subStartVol, \
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
          \ subDistId     \
          \ FROM input_dynamic_subscribers"
        
    somedata :: Handler String
    somedata = return "Jai Guru Maa from new API"
                         
                         
                       