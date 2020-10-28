{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Adapter.HTTP.Handlers.NewApplication where

import Adapter.HTTP.Api
import Control.Concurrent ()
import Control.Exception (bracket)
import Control.Monad ()
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List (intercalate)
import Data.String (fromString)
import Data.Maybe ( fromJust )
import Data.Pool ( Pool, withResource )
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types
import Servant ( Handler )
import Types


newApplication :: 
  Pool Connection ->
  ApplicationType ->
  Subscriber ->
  Handler SubscriberApplication
newApplication conns appType subscriber = do
  res <- liftIO . withResource conns $ \conn ->
   query conn
    "INSERT INTO input_dynamic_subscriber_applications( \
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
    \ subEndVol,    \
    \ appType ) VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?) \
    \ RETURNING \
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
    \  subEndVol"
    [ subscriberCode
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
    , show <$> subEndVol subscriber 
    , Just $ show appType ]
  return $ head res
  where
    subscriberCode = case appType of
      AddNewSubscriber -> Just "TEMP"
      EditDetails      -> subId subscriber
      RenewSubscriber  -> subId subscriber

