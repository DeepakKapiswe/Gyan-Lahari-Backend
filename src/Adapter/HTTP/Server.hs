{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Adapter.HTTP.Server where

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

import Adapter.HTTP.Servers.Subscriber
import Adapter.HTTP.Servers.Distributor
import Adapter.HTTP.Servers.AuthHandler
import Adapter.HTTP.Servers.Approver (pApproverServer)
import Adapter.HTTP.Handlers.FilterSubscribers
    ( filterSubscribers )
import Adapter.HTTP.Handlers.NewApplication (newApplication)


server ::
     Pool Connection
  -> CookieSettings
  -> JWTSettings
  -> Server (API auths)
server conns cs jwts =
       pSubscriberServer conns
  :<|> pDistributorServer conns
  :<|> pApproverServer conns
  :<|> protected conns
  :<|> logout cs
  :<|> authHandler conns cs jwts
  :<|> arogyam conns


logout ::
     CookieSettings
  -> AuthResult (AllowedUserRoles AllUsers)
  -> Server LogoutAPI
logout cs _ = return $ clearSession cs "Logged Out"

arogyam :: Pool Connection -> Server ArogyamAPI
arogyam conns =
  getSlots :<|>
  bookSlot :<|>
  viewBooking :<|> 
  getAllBookings
  where
    getSlots :: Handler [Slot]
    getSlots =
      liftIO $ withResource conns $ \conn ->
        query_ conn "SELECT     \
        \ slotId, \ 
        \ day, \
        \ slotTime, \
        \ bookingStatus, \
        \ patientName, \
        \ mobile, \
        \ city\
        \ From slots \
        \ Order By slotId Asc"

    bookSlot :: Slot -> Handler [Slot]    
    bookSlot slot = do
      -- warning dirty fix for checking 
      -- should not be run with empty mobile
      lookupRes <- viewBooking (SlotQuery $ fromJust $ mobile slot)
      case lookupRes of
        (x:_) -> return [x,x]
        [] ->
          liftIO $ withResource conns $ \conn ->
            query conn 
              "UPDATE slots \
              \ SET \
              \ bookingStatus = ?, \
              \ patientName = ?, \
              \ mobile = ?, \
              \ city =? \
                \ WHERE \
              \ slotId = ? \
                \ RETURNING \
                  \ slotId, \ 
                  \ day, \
                  \ slotTime, \
                  \ bookingStatus, \
                  \ patientName, \
                  \ mobile, \
                  \ city"
              [ pure "Booked"
              , patientName slot
              , mobile slot
              , city slot
              , show <$> slotId slot]
              

    viewBooking :: SlotQuery -> Handler [Slot]
    viewBooking sq = 
      liftIO $ withResource conns $ \conn ->
        query conn
          "SELECT \
            \ slotId, \ 
            \ day, \
            \ slotTime, \
            \ bookingStatus, \
            \ patientName, \
            \ mobile, \
            \ city \
          \ From slots\
          \ WHERE \
            \ mobile = ?"
          [sqMobile sq]
    
    getAllBookings :: Handler [Slot]
    getAllBookings =
      liftIO $ withResource conns $ \conn ->
        query_ conn "SELECT     \
        \ slotId, \ 
        \ day, \
        \ slotTime, \
        \ bookingStatus, \
        \ patientName, \
        \ mobile, \
        \ city\
        \ From slots \
        \  WHERE \
        \ bookingStatus = 'Booked' \
        \ Order By slotId Asc"

protected ::
     Pool Connection 
  -- -> R.Connection 
  -> AuthResult (AllowedUserRoles '[UManager, UApprover, UAdmin])
  -> Server ProtectedAPI
protected a (Authenticated user) = serverP a
protected _ x = throwAll err401 { errBody = BL.pack $ show x }


serverP :: Pool Connection -> Server ProtectedAPI
serverP conns =
  getAllSubscriber         :<|> 
  getDistributor           :<|>
  getAllDistributor        :<|>
  distSubscribers          :<|>
  distributionList         :<|>
  bulkDistributionList     :<|>
  expiryList               :<|>
  bulkExpiryList           :<|>
  searchSubscriber         :<|>
  recentlyAddedSubscribers :<|>
  filterSubscribers conns  :<|>
  getAllSubscriberApplications :<|>
  newApplication conns AddNewSubscriber :<|>
  newApplication conns EditDetails :<|>
  getSubscriber
  where
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
         \  subId = ?"
        [subId]

    getAllSubscriber :: Handler [Subscriber]
    getAllSubscriber =
      liftIO $ withResource conns $ \conn ->
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
        \ subDistId,    \
        \ subEndVol     \
        \ FROM input_dynamic_subscribers \
        \ ORDER BY \
          \ substate, \
          \ subcity,  \
          \ subpost,  \
          \ subpincode, \
          \ subadd2,  \
          \ subadd1,  \
          \ subabout, \
          \ subname   \
          \ LIMIT 200 "
    

    
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
    
    getDistributor :: DistributorId -> Handler Distributor
    getDistributor distId = do 
      res <- liftIO $
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
            [distId]
      return $ head res

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
        \      subDistId = ? \
        \    ORDER BY \
             \ substate, \
             \ subcity,  \
             \ subpost,  \
             \ subpincode, \
             \ subadd2,  \
             \ subadd1,  \
             \ subabout, \
             \ subname"
      [distId distributor]

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
        [ dldDistId dlDetails
        , show <$> dldCurrentVol dlDetails 
        , show <$> dldCurrentVol dlDetails ]
      
      dist <- getDistributor . fromJust $ dldDistId dlDetails
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
    
    bulkDistributionList :: BulkDistributionListDetails -> Handler [DistributionList]
    bulkDistributionList bdlDetails =
      forM allDlDetails distributionList
      where
        allDlDetails = bulkDistributionDetailsToList bdlDetails

    
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
        [ eldDistId elDetails
        , show <$> eldExpiryVol elDetails
        , show <$> pure expiryLowestVol ]
      
      dist <- getDistributor . fromJust $ eldDistId elDetails
      let expiryCount  = length expiries
      return $
        ExpiryList
          dist
          expiryVol
          expiryYearDuration
          expiryCount
          expiries
          
 

    bulkExpiryList :: BulkExpiryListDetails -> Handler [ExpiryList]
    bulkExpiryList belDetails =
      forM allElDetails expiryList
      where
        allElDetails = bulkExpiryListDetailsToList belDetails
      
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
        \ ORDER BY subId DESC \
        \ LIMIT ?"
        [show count]
    
    getAllSubscriberApplications :: Handler [SubscriberApplication]
    getAllSubscriberApplications = liftIO $ 
      withResource conns $ \conn ->
        query_ conn "SELECT     \
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
          \ ORDER BY subAppId DESC"

    -- execRedisIO :: R.Redis a -> IO a
    -- execRedisIO a = R.runRedis redConn a

                     
a = undefined


bulkDistributionDetailsToList
  :: BulkDistributionListDetails
  -> [DistributionListDetails]
bulkDistributionDetailsToList
  (BulkDistributionListDetails distIds cv) =
  case distIds of
    Nothing -> []
    Just dIds -> DistributionListDetails . Just <$> dIds <*> pure cv

bulkExpiryListDetailsToList
  :: BulkExpiryListDetails
  -> [ExpiryListDetails]
bulkExpiryListDetailsToList
  (BulkExpiryListDetails distIds ev eYd) =
    case distIds of
      Nothing   -> []
      Just dIds -> ExpiryListDetails . Just <$> dIds <*> pure ev <*> pure eYd

-- ------------------------------------------------------------------------------------
