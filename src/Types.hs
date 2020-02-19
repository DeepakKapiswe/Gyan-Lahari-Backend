{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Types where
    
import Data.Aeson
import Database.PostgreSQL.Simple (FromRow)
import GHC.Generics (Generic)

data CustAddress = CustAddress {
    custSaluation :: String
  , custFname     :: String
  , custMname     :: String
  , custLname     :: String
  , custAbout     :: String
  , custAdd1      :: String
  , custAdd2      :: String
  , custPost      :: String
  , custCity      :: String
  , custState     :: String
  , custPincode   :: String
  , custPhone     :: String
} deriving (Show, Eq, Generic, FromRow)


instance FromJSON CustAddress
instance ToJSON CustAddress