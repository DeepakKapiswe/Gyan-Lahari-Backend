{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module App where

import           Data.Aeson
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors
import           Servant
import           System.IO

-- * api

type ItemApi =
  "item" :> Get '[JSON] Item :<|>
  "item" :> Capture "itemId" Integer :> Get '[JSON] Item :<|>
  "item" :> "add" :> ReqBody '[JSON] Item :> Post '[JSON] Int

itemApi :: Proxy ItemApi
itemApi = Proxy

-- * app

run :: IO ()
run = do
  let port = 7000
      settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
        defaultSettings
  runSettings settings =<< mkApp

mkApp :: IO Application
mkApp = return . simpleCors $ serve itemApi server

server :: Server ItemApi
server =
  getItems :<|>
  getItemById :<|>
  postItem

getItems :: Handler Item
getItems = return exampleItem

getItemById :: Integer -> Handler Item
getItemById = \ case
  0 -> return exampleItem
  _ -> throwError err404

postItem :: Item -> Handler Int
postItem _ = return 1000

exampleItem :: Item
exampleItem = Item 0 "example item"

-- * item

data Item
  = Item {
    itemId :: Integer,
    itemText :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON Item
instance FromJSON Item

data a + b = Foo a b

type X = Int + Bool
