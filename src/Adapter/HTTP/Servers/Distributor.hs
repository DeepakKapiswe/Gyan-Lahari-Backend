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

-- import qualified Database.Redis as R
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL

import Servant.Auth.Server

import Adapter.HTTP.Api
import Types
import Adapter.HTTP.Handlers.FilterSubscribers
    ( filterSubscribers )
import Adapter.HTTP.Handlers.NewApplication (newApplication)


pDistributorServer ::
  Pool Connection
  -> AuthResult (AllowedUserRoles '[UDistributor])
  -> Server DistributorAPI
pDistributorServer pConns (Authenticated (AllowedUser usr)) = distributorServer pConns usr
pDistributorServer _ x = throwAll err401 { errBody = BL.pack $ show x }
 



distributorServer :: Pool Connection -> User -> Server DistributorAPI
distributorServer conns usr = 
       getDistributor
  :<|> distributionList
  :<|> expiryList
  :<|> searchSubscriber
  :<|> recentlyAddedSubscribers
  :<|> getAllSubscriber
  :<|> distFilterSubscribers
  :<|> getAllSubscriberApplications
  :<|> addNewSubscriber 
  :<|> editDetails
  :<|> getSubscriber


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
    
    distributionList :: DistributionListDetails -> Handler DistributionList
    distributionList dlDetails = do
      subs <- liftIO $ withResource conns $ \conn ->
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
        \      subEndVol   >= ? \
        \    ORDER BY \
             \ substate, \
             \ subcity,  \
             \ subpost,  \
             \ subpincode, \
             \ subadd2,  \
             \ subadd1,  \
             \ subabout, \
             \ subname"  
        [ uId usr
        , show <$> dldCurrentVol dlDetails 
        , show <$> dldCurrentVol dlDetails ]
      
      dist <- head <$> getDistributor 
      let currVol = fromJust $ dldCurrentVol dlDetails
          currExpList = filter (\sub -> subEndVol sub == dldCurrentVol dlDetails) subs
          runningCount = length subs
          expiryCount  = length currExpList
      return $
        DistributionList
          dist
          currVol
          runningCount
          expiryCount
          currExpList
          subs
    
    expiryList :: ExpiryListDetails -> Handler ExpiryList
    expiryList elDetails = do
      let expiryVol = fromJust $ eldExpiryVol elDetails
          expiryYearDuration = fromJust $ eldExpiryYearDuration elDetails
          expiryLowestVol = expiryVol - (4 * expiryYearDuration + rem expiryVol 4) 
      expiries <- liftIO $ withResource conns $ \conn ->
        query conn "\
        \    SELECT         \
        \     subId,        \
        \     subStartVol,  \
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
        \     WHERE             \
        \      subDistId = ?    \
        \        AND            \
        \      subEndVol < ?    \
        \       AND             \
        \      subEndVol   >= ? \
        \    ORDER BY \
             \ substate, \
             \ subcity,  \
             \ subpost,  \
             \ subpincode, \
             \ subadd2,  \
             \ subadd1,  \
             \ subabout, \
             \ subname"  
        [ uId usr
        , show <$> eldExpiryVol elDetails
        , show <$> pure expiryLowestVol ]

      -- TODO: this can be optimised somehow as we don't need to fetch distributor
      -- details again and again, maybe cache storage or something
      dist <- head <$> getDistributor
      let expiryCount  = length expiries
      return $
        ExpiryList
          dist
          expiryVol
          expiryYearDuration
          expiryCount
          expiries
    
    searchSubscriber :: SearchQuery -> Handler [Subscriber]
    searchSubscriber sq = liftIO $
      withResource conns $ \conn ->
        query
          conn
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
          \     WHERE subDistId = ? \
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
          (sqSubName sq, fromJust $ uId usr, sqLimit sq)
    
    -- TODO : The logic of Recently Added Subscribers has to be changed
    recentlyAddedSubscribers :: Int -> Handler [Subscriber]
    recentlyAddedSubscribers count = liftIO $
      withResource conns $ \conn ->
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
          \ WHERE subdistId = ? \
        \ ORDER BY subId DESC \
        \ LIMIT ?"
        [ fromJust $ uId usr, show count]
    
    getAllSubscriber :: Handler [Subscriber]
    getAllSubscriber =
      liftIO $ withResource conns $ \conn ->
        query conn "SELECT    \
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
        \   WHERE subDistId = ? \
        \ ORDER BY \
          \ substate, \
          \ subcity,  \
          \ subpost,  \
          \ subpincode, \
          \ subadd2,  \
          \ subadd1,  \
          \ subabout, \
          \ subname"
        [uId usr]
    
    distFilterSubscribers :: FilterOptions -> Handler [Subscriber]
    distFilterSubscribers fo = filterSubscribers conns fo'
      where
        fo' = fo {foSubDistId = pure <$> uId usr }
    
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
          \   WHERE subDistId = ? \
          \ ORDER BY subAppId DESC"
        [uId usr]
    
    addNewSubscriber :: Subscriber -> Handler SubscriberApplication
    addNewSubscriber sub = newApplication conns AddNewSubscriber sub'
      where
        sub' = sub {subDistId = uId usr}
    
    editDetails :: Subscriber -> Handler SubscriberApplication
    editDetails sub = newApplication conns EditDetails sub'
      where
        sub' = sub {subDistId = uId usr}
    
    getSubscriber :: SubId -> Handler [Subscriber]
    getSubscriber subId = 
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
         \ WHERE  \
         \  subId = ? \
         \ AND \
           \subDistId = ?"
        [ pure subId
        , uId usr ]

