{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
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

import qualified Data.UUID as U
import Data.UUID.V4
import qualified Database.Redis as R
import qualified Data.ByteString.Char8 as B

import Servant.Auth.Server
-- import Servant.Auth.Server.SetCookieOrphan ()

import Adapter.HTTP.Api
import Types


server ::
     Pool Connection
  -> R.Connection
  -> CookieSettings
  -> JWTSettings
  -> Server (API auths)
server a b cs jwts =
       protected a b
  :<|> unprotected cs jwts

unprotected :: CookieSettings -> JWTSettings -> Server UnProtectedAPI
unprotected = checkCreds

protected ::
     Pool Connection 
  -> R.Connection 
  -> AuthResult UserAuth
  -> Server ProtectedAPI
protected a b (Authenticated user) = serverP a b 
protected _ _ _ = throwAll err401

-- Here is the login handler
checkCreds :: CookieSettings
           -> JWTSettings
           -> UserAuth
           -> Handler (Headers '[ Header "Set-Cookie" SetCookie
                                , Header "Set-Cookie" SetCookie]
                               NoContent)
checkCreds cookieSettings jwtSettings usr@(UserAuth name pass) = do
   -- Usually you would ask a database for the user info. This is just a
   -- regular servant handler, so you can follow your normal database access
   -- patterns (including using 'enter').
  --  let usr = UserAuth "Ali Baba" "ali@email.com"
   liftIO $ print usr
   mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings usr
   case mApplyCookies of
     Nothing           -> throwError err401
     Just applyCookies -> return $ applyCookies NoContent
checkCreds _ _ _ = do
  liftIO $ print "In Check Cred Error Branch"
  throwError err401

serverP :: Pool Connection -> R.Connection -> Server ProtectedAPI
serverP conns redConn =
  postSubscriber       :<|> 
  getAllSubscriber     :<|> 
  updateSubscriber     :<|>
  postDistributor      :<|>
  getDistributor       :<|>
  getAllDistributor    :<|>
  updateDistributor    :<|>
  distSubscribers      :<|>
  distributionList     :<|>
  bulkDistributionList :<|>
  expiryList           :<|>
  bulkExpiryList       :<|>
  searchSubscriber     :<|>
  recentlyAddedSubscribers :<|>
  checkUserAuth
  where
    postSubscriber :: Subscriber -> Handler Subscriber
    postSubscriber subscriber = do
      res <- liftIO . withResource conns $ \conn -> 
       query conn
        "INSERT INTO input_dynamic_subscribers( \
        \ subId,        \
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
        
    getAllSubscriber :: Handler [Subscriber]
    getAllSubscriber = do
      a <- liftIO . execRedisIO $ R.get "hari" 
      liftIO $ print a
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
          \         levenshtein _less_equal(_res3.subname,(select * from _name), 1,0,1,7) asc \
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
    
    checkUserAuth :: UserAuth -> Handler Bool
    checkUserAuth (UserAuth ui pass) = do
      (res::[UserAuth]) <- liftIO $ withResource conns $ \conn ->
        query conn "SELECT \
        \ userId, \
        \ userPassword \
        \ FROM userLogin \
        \ WHERE \
        \   userId = ? \
        \    AND \
        \   userPassword = ?"
          [ ui, pass]
      liftIO . execRedisIO $ do
        bs <- liftIO $ nextRandom
        R.set "hari" (U.toASCIIBytes bs)
      return (res /= [])

    execRedisIO :: R.Redis a -> IO a
    execRedisIO a = R.runRedis redConn a

                     
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
