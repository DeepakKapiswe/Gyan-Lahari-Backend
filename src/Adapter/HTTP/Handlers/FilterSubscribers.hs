{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Adapter.HTTP.Handlers.FilterSubscribers where

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

filterSubscribers ::
  Pool Connection ->
  FilterOptions ->
  Handler
    [Subscriber]
filterSubscribers conns fo = liftIO $
  withResource conns $ \conn -> do
    let (queryStrModifier, params) = filterOptionsToQueryStrAndParams fo
    query
      conn
      ("    SELECT \
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
      \      WHERE"  <> fromString queryStrModifier  <> orderingStr)
      (In <$> params)
      


filterOptionsToQueryStrAndParams :: FilterOptions -> (String, [[String]])
filterOptionsToQueryStrAndParams fo = (queryStr, queryParams)
  where
    queryStr = intercalate " AND " . fst $ unzip valuesWithFieldNames
    queryParams = snd <$> valuesWithFieldNames
    valuesWithFieldNames = fmap (fmap fromJust) . filter ((/= Nothing) . snd) $ zip foFieldNames maybeValues
    maybeValues = ($ fo) <$>  foFunctions
    foFunctions = [ foSubId
                  , fmap (show <$>) . foSubStartVol
                  , fmap (show <$>) . foSubSubscriptionType
                  , fmap (show <$>) . foSubSlipNum
                  , foSubName
                  , foSubAbout
                  , foSubAdd1
                  , foSubAdd2
                  , foSubPost
                  , foSubCity
                  , foSubState
                  , foSubPincode
                  , foSubPhone
                  , foSubRemark
                  , foSubDistId
                  , fmap (show <$>). foSubEndVol
                  ]
    foFieldNames = [ " subId in ?"
                   , " subStartVol in ?"
                   , " subSubscriptionType in ?"
                   , " subSlipNum in ?"
                   , " subName in ?"
                   , " subAbout in ?"
                   , " subAdd1 in ?"
                   , " subAdd2 in ?"
                   , " subPost in ?"
                   , " subCity in ?"
                   , " subState in ?"
                   , " subPincode in ?"
                   , " subPhone in ?"
                   , " subRemark in ?"
                   , " subDistId in ?"
                   , " subEndVol in ?"
                   ]

orderingStr =
  " ORDER BY \
    \ substate, \
    \ subcity,  \
    \ subpost,  \
    \ subpincode, \
    \ subadd2,  \
    \ subadd1,  \
    \ subabout, \
    \ subname"