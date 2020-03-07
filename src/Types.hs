{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Types where
    
import Data.Aeson
import Database.PostgreSQL.Simple (FromRow, ToRow)
import GHC.Generics (Generic)

data Subscriber = Subscriber {
    subStartVol  :: Maybe String
  , subSubscriptionType  :: Maybe String
  , subSlipNum   :: Maybe String
  , subName      :: Maybe String
  , subAbout     :: Maybe String
  , subAdd1      :: Maybe String
  , subAdd2      :: Maybe String
  , subPost      :: Maybe String
  , subCity      :: Maybe String
  , subState     :: Maybe String
  , subPincode   :: Maybe String
  , subPhone     :: Maybe String
  , subRemark    :: Maybe String
  , subDistId    :: Maybe String
} deriving (Show, Eq, Generic, FromRow)


instance FromJSON Subscriber
instance ToJSON Subscriber

data Distributor = Distributor {
    distId    :: Maybe String
  , distName  :: Maybe String
  , distAdd   :: Maybe String
  , distCity  :: Maybe String
  , distPhone :: Maybe String
} deriving (Show, Eq, Generic, FromRow)

instance FromJSON Distributor
instance ToJSON Distributor

data SearchQuery = SearchQuery {
    -- sqDistId  :: String
   sqSubName :: String
  -- , sqQuery   :: String
  , sqLimit   :: String
} deriving (Show, Eq, Generic, FromRow, ToRow)

instance FromJSON SearchQuery
instance ToJSON SearchQuery