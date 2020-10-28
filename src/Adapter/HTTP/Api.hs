{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

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
      ("sub" :> Auth auths (AllowedUserRoles '[USubscriber]) :> SubscriberAPI) 
    :<|> 
      ("dist" :> Auth auths (AllowedUserRoles '[UDistributor]) :> DistributorAPI) 
    :<|>
      (Auth auths (AllowedUserRoles '[UApprover, UAdmin]) :> ApproverAPI)
    :<|>
      (Auth auths (AllowedUserRoles '[UManager, UApprover, UAdmin]) :> ProtectedAPI)
    :<|> 
      (Auth auths (AllowedUserRoles AllUsers) :> LogoutAPI)
    :<|> 
      UnProtectedAPI
    )

type LogoutAPI =
    "logout"
      :> Get '[JSON] (AuthCookies String)

type UnProtectedAPI =
    "login"
      :> ReqBody '[JSON] UserAuth
      :> Verb 'POST 200 '[JSON] (AuthCookies User)
  :<|>
    "distLogin"
      :> ReqBody '[JSON] UserAuth
      :> Verb 'POST 200 '[JSON] (AuthCookies (Maybe Distributor))

type SubscriberAPI =
    "viewSubscriber"
      :> Get '[JSON] [Subscriber]
  :<|>
    "viewAllSubscriberApplications"
      :> Get '[JSON] [SubscriberApplication]  
  :<|>
    "applyForEditSubscriber"
      :> ReqBody '[JSON] Subscriber
      :> Post '[JSON] SubscriberApplication

type DistributorAPI =
    "viewDistributor"
      :> Get '[JSON] [Distributor]
  :<|>
    "distributionList"
      :> ReqBody '[JSON] DistributionListDetails
      :> Post '[JSON] DistributionList
  :<|>
    "expiryList"
      :> ReqBody '[JSON] ExpiryListDetails
      :> Post '[JSON] ExpiryList
  :<|>
    "searchSubscriber"
      :> ReqBody '[JSON] SearchQuery
      :> Post '[JSON] [Subscriber]
  :<|>
    "recentlyAddedSubscribers"
      :> ReqBody '[JSON] Int
      :> Post '[JSON] [Subscriber]
  :<|>
    "getAllSubscribers"
      :> Get '[JSON] [Subscriber]
  :<|>
    "filterSubscribers"
      :> ReqBody '[JSON] FilterOptions
      :> Post '[JSON] [Subscriber]
  :<|> 
    "getAllSubscriberApplications"
      :> Get '[JSON] [SubscriberApplication]
  :<|>
    "applyForNewSubscriber"
      :> ReqBody '[JSON] Subscriber
      :> Post '[JSON] SubscriberApplication
  :<|>
    "applyForUpdateSubscriber"
      :> ReqBody '[JSON] Subscriber
      :> Post '[JSON] SubscriberApplication
  :<|>
    "getSubscriber"
      :> ReqBody '[JSON] SubId
      :> Post '[JSON] [Subscriber]

type ApproverAPI = 
    "addUser"
      :> ReqBody '[JSON] Subscriber
      :> Post '[JSON] Subscriber
  :<|>
    "updateSubscriber"
      :> ReqBody '[JSON] Subscriber
      :> Post '[JSON] String
  :<|>
    "addDistributor"
      :> ReqBody '[JSON] Distributor
      :> Post '[JSON] Distributor
  :<|>
    "updateDistributor"
      :> ReqBody '[JSON] Distributor
      :> Post '[JSON] String
  :<|>
    "approveSubscriberApplication"
      :> ReqBody '[JSON] ApprovalRequest
      :> Post '[JSON] SubscriberApplication
  :<|>
    "rejectSubscriberApplication"
      :> ReqBody '[JSON] ApprovalRequest
      :> Post '[JSON] SubscriberApplication


type ProtectedAPI =
    "getAllSubscribers"
      :> Get '[JSON] [Subscriber]
  :<|>  
    "getDistributor"
      :> ReqBody '[JSON] DistributorId
      :> Post '[JSON] Distributor
  :<|>
    "getAllDistributor"
      :> Get '[JSON] [Distributor]
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
    "filterSubscribers"
      :> ReqBody '[JSON] FilterOptions
      :> Post '[JSON] [Subscriber]
  :<|> 
    "getAllSubscriberApplications"
      :> Get '[JSON] [SubscriberApplication]
  :<|>
    "applyForNewSubscriber"
      :> ReqBody '[JSON] Subscriber
      :> Post '[JSON] SubscriberApplication
  :<|>
    "applyForUpdateSubscriber"
      :> ReqBody '[JSON] Subscriber
      :> Post '[JSON] SubscriberApplication
  :<|>
    "getSubscriber"
      :> ReqBody '[JSON] SubId
      :> Post '[JSON] [Subscriber]