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
  postSubscriber    :<|> 
  getAllSubscriber  :<|> 
  updateSubscriber  :<|>
  postDistributor   :<|>
  getAllDistributor :<|>
  updateDistributor :<|>
  distSubscribers   :<|>
  distributionList  :<|>
  searchSubscriber    
  where
    postSubscriber :: Subscriber -> Handler String
    postSubscriber subscriber = do
      liftIO . withResource conns $ \conn ->
        execute conn
        "INSERT INTO input_dynamic_subscribers( \    
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
        \ subEndVol     ) VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"
        [ show <$> subStartVol subscriber
        , show <$> subSubscriptionType subscriber
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
        , subDistId  subscriber
        , show <$> subEndVol  subscriber ]
      return "Data Tried to add on Database"
        
    getAllSubscriber :: Handler [Subscriber]
    getAllSubscriber = liftIO $ 
      withResource conns $ \conn ->
        query_ conn "SELECT    \
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
        \ subDistId,     \
        \ subEndVol     \
        \ FROM input_dynamic_subscribers LIMIT 200"
    
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
        , subDistId  subscriber 
        , show <$> subEndVol  subscriber
        , subId      subscriber ]
      return "Data Updated on Database"
    
    postDistributor :: Distributor -> Handler String
    postDistributor distributor = do
      liftIO . withResource conns $ \conn ->
        execute conn
          "INSERT INTO input_static_distributors( \    
           \ distId,    \
           \ distName,  \
           \ distAdd,   \
           \ distCity,  \
           \ distPhone  \
           \ ) VALUES (?,?,?,?,?)"
        [ distId distributor
        , distName distributor
        , distAdd distributor
        , distCity distributor
        , distPhone distributor ]
      return "Data Tried to add on Database"

    getAllDistributor :: Handler [Distributor]
    getAllDistributor = liftIO $
      withResource conns $ \conn ->
        query_ conn "SELECT  \
          \ distId,    \
          \ distName,  \
          \ distAdd,   \
          \ distCity,  \
          \ distPhone  \
          \ FROM input_static_distributors"
         
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

    distSubscribers :: Distributor -> Handler [Subscriber]
    distSubscribers distributor = liftIO $
      withResource conns $ \conn ->
        query conn "\
        \    SELECT \
        \     subId,       \
        \     subStartVol, \
        \     subSubscriptionType, \
        \     subSlipNum,   \
        \     subName,      \
        \     subAbout,     \
        \     subAdd1,      \
        \     subAdd2,      \
        \     subPost,      \
        \     subCity,      \
        \     subState,     \
        \     subPincode,   \
        \     subPhone,     \
        \     subRemark,    \
        \     subDistId,    \
        \     subEndVol     \   
        \    FROM input_dynamic_subscribers \
        \     WHERE \
        \      subDistId = ? "
      [distId distributor]

    distributionList :: DistributionListDetails -> Handler [Subscriber]
    distributionList dlDetails = liftIO $
      withResource conns $ \conn ->
        query conn "\
        \    SELECT \
        \     subId,       \
        \     subStartVol, \
        \     subSubscriptionType, \
        \     subSlipNum,   \
        \     subName,      \
        \     subAbout,     \
        \     subAdd1,      \
        \     subAdd2,      \
        \     subPost,      \
        \     subCity,      \
        \     subState,     \
        \     subPincode,   \
        \     subPhone,     \
        \     subRemark,    \
        \     subDistId,    \
        \     subEndVol     \   
        \    FROM input_dynamic_subscribers \
        \     WHERE \
        \      subDistId = ?    \
        \        AND            \
        \      subStartVol <= ? \
        \       AND             \
        \      subEndVol   >= ? "  
      [ dldDistId dlDetails
      , show <$> dldCurrentVol dlDetails 
      , show <$> dldCurrentVol dlDetails ]
      
    searchSubscriber :: SearchQuery -> Handler [Subscriber]
    searchSubscriber sq = liftIO $
      withResource conns $ \conn ->
        query conn
          "with \
          \  _name as (SELECT ?), \
          \  _fname as (SELECT split_part((select * from _name), ' ', 1)), \
          \  _res1 as ( \
          \    SELECT \
          \     subId,       \
          \     subStartVol, \
          \     subSubscriptionType, \
          \     subSlipNum,   \
          \     subName,      \
          \     subAbout,     \
          \     subAdd1,      \
          \     subAdd2,      \
          \     subPost,      \
          \     subCity,      \
          \     subState,     \
          \     subPincode,   \
          \     subPhone,     \
          \     subRemark,    \
          \     subDistId,    \
          \     subEndVol     \    
          \    FROM input_dynamic_subscribers \
          \    ORDER BY \
          \      levenshtein_less_equal(subname,(select * from _name), 1,0,1,7) asc \
          \    LIMIT 1000 \
          \    ), \
          \  _res2 as (  \
          \    SELECT * from _res1 \
          \      ORDER BY  \
          \        metaphone((select * from _name),25) <<-> (metaphone(_res1.subName,25)) ASC \
          \  ), \
          \  _res3 as (  \
          \    SELECT * from _res2 \
          \      ORDER BY  \
          \        (select * from _fname) <<-> _res2.subname ASC \
          \  ), \
          \  _res4 as (  \
          \    SELECT * from _res3 \
          \      ORDER BY  \
          \        (select min(levenshtein_less_equal(name,(select * from _fname),2,1,1,7)) from unnest(string_to_array(_res3.subname, ' ')) as name), \
          \         levenshtein_less_equal(_res3.subname,(select * from _name), 1,0,1,7) asc \
          \       LIMIT ? ) \
          \  SELECT * from _res4"
        (sqSubName sq, sqLimit sq)

        
                         
                         
              