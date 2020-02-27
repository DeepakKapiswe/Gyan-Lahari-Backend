{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Types where
    
import Data.Aeson
import Database.PostgreSQL.Simple (FromRow)
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