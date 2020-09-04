{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module Adapter.HTTP.Api where

import Servant
import Data.Aeson
import GHC.Generics
import Servant.Auth.Server

import Types

api :: Proxy (API '[Cookie])
api = Proxy

type AuthCookies = Headers '[ Header "Set-Cookie" SetCookie
                            , Header "Set-Cookie" SetCookie]

type API auths =
  "api" :>
    (
      (Auth auths (AllowedUserRoles '[USubscriber]) :> SubscriberAPI) 
    :<|> 
      (Auth auths (AllowedUserRoles '[UDistributor]) :> DistributorAPI) 
    :<|> 
      (Auth auths (UserAtLeast 'UManager) :> ProtectedAPI)
    :<|> 
      UnProtectedAPI
    )

type UnProtectedAPI = "login"
    :> ReqBody '[JSON] UserAuth
    :> Verb 'POST 200 '[JSON] (AuthCookies User)

type SubscriberAPI =
  "viewSubscriber"
      :> Get '[JSON] [Subscriber]

type DistributorAPI =
    "viewDistributor"
      :> Get '[JSON] [Distributor]


type ProtectedAPI =
    "addUser"
      :> ReqBody '[JSON] Subscriber
      :> Post '[JSON] Subscriber
  :<|> 
    "getAllSubscribers"
      :> Get '[JSON] [Subscriber]
  :<|>
    "updateSubscriber"
      :> ReqBody '[JSON] Subscriber
      :> Post '[JSON] String
  :<|> 
    "addDistributor"
      :> ReqBody '[JSON] Distributor
      :> Post '[JSON] Distributor
  :<|> 
    "getDistributor"
      :> ReqBody '[JSON] DistributorId
      :> Post '[JSON] Distributor
  :<|>
    "getAllDistributor"
      :> Get '[JSON] [Distributor]
  :<|>
    "updateDistributor"
      :> ReqBody '[JSON] Distributor
      :> Post '[JSON] String
  :<|>
    "distSubscribers"
      :> ReqBody '[JSON] Distributor
      :> Post '[JSON] [Subscriber]
  :<|>
    "distributionList"
      :> ReqBody '[JSON] DistributionListDetails
      :> Post '[JSON] DistributionList
  :<|>
    "bulkDistributionList"
      :> ReqBody '[JSON] BulkDistributionListDetails
      :> Post '[JSON] [DistributionList] 
  :<|>
    "expiryList"
      :> ReqBody '[JSON] ExpiryListDetails
      :> Post '[JSON] ExpiryList
  :<|>
    "bulkExpiryList"
      :> ReqBody '[JSON] BulkExpiryListDetails
      :> Post '[JSON] [ExpiryList]
  :<|>
    "searchSubscriber"
      :> ReqBody '[JSON] SearchQuery
      :> Post '[JSON] [Subscriber]
  :<|>
    "recentlyAddedSubscribers"
      :> ReqBody '[JSON] Int
      :> Post '[JSON] [Subscriber]
  :<|>
    "checkUserAuth"
      :> ReqBody '[JSON] UserAuth
      :> Post '[JSON] Bool


