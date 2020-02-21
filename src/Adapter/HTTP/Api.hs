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
    "user" :> Get '[JSON] String

api :: Proxy (API :<|> Raw)
api = Proxy
