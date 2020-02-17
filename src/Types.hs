{-# LANGUAGE DeriveGeneric #-}

module Types where
    
import Data.Aeson
import GHC.Generics

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
} deriving (Show, Eq, Generic)


instance FromJSON CustAddress
instance ToJSON CustAddress