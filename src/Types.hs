{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Types where
    
import Data.Aeson
import Database.PostgreSQL.Simple (FromRow, ToRow)
import GHC.Generics (Generic)

data Subscriber = Subscriber {
    subId        :: Maybe String
  , subStartVol  :: Maybe Int
  , subSubscriptionType  :: Maybe Int
  , subSlipNum   :: Maybe Int
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
  , subEndVol    :: Maybe Int
} deriving (Show, Eq, Generic, FromRow)


instance FromJSON Subscriber
instance ToJSON Subscriber


type DistributorId = String

data Distributor = Distributor {
    distId    :: Maybe String
  , distName  :: Maybe String
  , distAdd   :: Maybe String
  , distCity  :: Maybe String
  , distPhone :: Maybe String
} deriving (Show, Eq, Generic, FromRow)

instance FromJSON Distributor
instance ToJSON Distributor

data DistributionListDetails = DistributionListDetails {
    dldDistId     :: Maybe String
  , dldCurrentVol :: Maybe Int
} deriving (Show, Eq, Generic, FromRow)

instance FromJSON DistributionListDetails
instance ToJSON   DistributionListDetails

data BulkDistributionListDetails = BulkDistributionListDetails {
    bdldDistIds    :: Maybe [String]
  , bdldCurrentVol :: Maybe Int
} deriving (Show, Eq, Generic)

instance FromJSON BulkDistributionListDetails
instance ToJSON   BulkDistributionListDetails

data DistributionList = DistributionList {
    dlDistributor    :: Distributor
  , dlCurrentVol     :: Int
  , dlRunningCount   :: Int
  , dlExpiryCount    :: Int
  , dlExpiries       :: [Subscriber]
  , dlSubscriberList :: [Subscriber]  
} deriving (Show, Eq, Generic)

instance FromJSON DistributionList
instance ToJSON   DistributionList

data ExpiryListDetails = ExpiryListDetails {
    eldDistId             :: Maybe String
  , eldExpiryVol          :: Maybe Int
  , eldExpiryYearDuration :: Maybe Int
} deriving (Show, Eq, Generic, FromRow)

instance FromJSON ExpiryListDetails
instance ToJSON   ExpiryListDetails

data BulkExpiryListDetails = BulkExpiryListDetails {
    beldDistIds            :: Maybe [String]
  , beldExpiryVol          :: Maybe Int
  , beldExpiryYearDuration :: Maybe Int
} deriving (Show, Eq, Generic)

instance FromJSON BulkExpiryListDetails
instance ToJSON   BulkExpiryListDetails

data ExpiryList = ExpiryList {
    elDistributor        :: Distributor
  , elExpiryVol          :: Int
  , elExpiryYearDuration :: Int
  , elExpiryCount        :: Int
  , elExpiries           :: [Subscriber]
} deriving (Show, Eq, Generic)

instance FromJSON ExpiryList
instance ToJSON   ExpiryList

data SearchQuery = SearchQuery {
    -- sqDistId  :: String
   sqSubName :: String
  -- , sqQuery   :: String
  , sqLimit   :: String
} deriving (Show, Eq, Generic, FromRow, ToRow)

instance FromJSON SearchQuery
instance ToJSON SearchQuery