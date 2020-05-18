{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module Adapter.HTTP.Api where

import Servant
import Data.Aeson
import GHC.Generics

import Types

type API =
    "addUser"
      :> ReqBody '[JSON] Subscriber
      :> Post '[JSON] String
  :<|> 
    Get '[JSON] [Subscriber]
  :<|>
    "updateSubscriber"
      :> ReqBody '[JSON] Subscriber
      :> Post '[JSON] String
  :<|> 
    "addDistributor"
      :> ReqBody '[JSON] Distributor
      :> Post '[JSON] String
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

api :: Proxy (API :<|> Raw)
api = Proxy
