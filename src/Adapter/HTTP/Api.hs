{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module Adapter.HTTP.Api where

import Servant
import Data.Aeson
import GHC.Generics


newtype Message = MkMessage { m :: String }
  deriving (Show, Eq, Generic )

instance FromJSON Message
instance ToJSON Message

type API =
  "addUser"
    :> ReqBody '[JSON] Message
    :> Post '[JSON] Message -- NoContent
  :<|> 
    Get '[JSON] [Message]
  :<|>
    "user" :> Get '[JSON] String

api :: Proxy (API :<|> Raw)
api = Proxy
