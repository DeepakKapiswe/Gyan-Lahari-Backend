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
  getDistributor :<|>
  distDistributionList :<|>
  distExpiryList

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
    
    distDistributionList :: DistributionListDetails -> Handler DistributionList
    distDistributionList dlDetails = do
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
    
    distExpiryList :: ExpiryListDetails -> Handler ExpiryList
    distExpiryList elDetails = do
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