{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}


module Types where
    
import Data.Aeson
import Database.PostgreSQL.Simple (FromRow, ToRow)
import Database.PostgreSQL.Simple.FromRow
import GHC.Generics (Generic)
import Servant.Auth.Server
import Data.Proxy
import Data.Text (Text, pack)
import Data.Data

data Subscriber = Subscriber {
    subId        :: Maybe String
  , subStartVol  :: Maybe Int
  , subSubscriptionType :: Maybe Int
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

data FilterOptions = FilterOptions {
    foSubId        :: Maybe [ String ]
  , foSubStartVol  :: Maybe [ Int ]
  , foSubSubscriptionType :: Maybe [ Int ]
  , foSubSlipNum   :: Maybe [ Int ]
  , foSubName      :: Maybe [ String ]
  , foSubAbout     :: Maybe [ String ]
  , foSubAdd1      :: Maybe [ String ]
  , foSubAdd2      :: Maybe [ String ]
  , foSubPost      :: Maybe [ String ]
  , foSubCity      :: Maybe [ String ]
  , foSubState     :: Maybe [ String ]
  , foSubPincode   :: Maybe [ String ]
  , foSubPhone     :: Maybe [ String ]
  , foSubRemark    :: Maybe [ String ]
  , foSubDistId    :: Maybe [ String ]
  , foSubEndVol    :: Maybe [ Int ]
} deriving (Show, Eq, Generic, Data, Typeable)

instance FromJSON FilterOptions
instance ToJSON FilterOptions

data SubscriberApplication = SubscriberApplication {
    saApplicationId  :: Maybe Int
  , saAppStatus      :: Maybe String
  , saProcessedBy    :: Maybe String
  , saSubscriberData :: Subscriber
} deriving (Show, Eq, Generic)

instance FromJSON SubscriberApplication
instance ToJSON   SubscriberApplication

instance FromRow SubscriberApplication where
  fromRow = SubscriberApplication <$> 
    field <*>
    field <*>
    field <*> 
    fromRow


data ApprovalRequest = ApprovalRequest {
    arApplicationIds :: Maybe [Int]
  , arProcessedBy :: Maybe String 
} deriving (Show, Eq, Generic)

instance FromJSON ApprovalRequest
instance ToJSON   ApprovalRequest

data ApprovalResponse = ApprovalResponse {
    arApplication :: SubscriberApplication
  , arSubscriber  :: Subscriber
} deriving (Show, Eq, Generic)

instance FromJSON ApprovalResponse
instance ToJSON   ApprovalResponse


data UserAuth = UserAuth {
    userId   :: Maybe String
  , password :: Maybe String
  , userRole :: Maybe String
} deriving (Show, Eq, Generic, FromRow, ToRow)

instance FromJSON UserAuth
instance ToJSON UserAuth

data UserRole =
    UGuest
  | USubscriber
  | UDistributor
  | UManager
  | UApprover
  | UAdmin
  deriving (Eq, Show, Read, Ord, Generic)

type AllUsers = '[UGuest, USubscriber, UDistributor, UManager, UApprover, UAdmin]

instance FromJSON UserRole
instance ToJSON UserRole

class KnownUserRole (r :: UserRole) where
  knownUserRole :: Proxy r -> UserRole

instance KnownUserRole 'UGuest where
  knownUserRole _ = UGuest

instance KnownUserRole 'USubscriber where
  knownUserRole _ = USubscriber

instance KnownUserRole 'UDistributor where
  knownUserRole _ = UDistributor

instance KnownUserRole 'UManager where
  knownUserRole _ = UManager

instance KnownUserRole 'UApprover where
  knownUserRole _ = UApprover

instance KnownUserRole 'UAdmin where
  knownUserRole _ = UAdmin

data User = 
  User {
    uId   :: Maybe String,
    uType :: UserRole
    }
  deriving (Eq, Show, Generic)

instance FromJSON User
instance ToJSON User
instance FromJWT User
instance ToJWT User

class KnownUserRoles (rs :: [UserRole]) where
  knownUserRoles :: Proxy rs -> [UserRole]

instance KnownUserRoles '[] where
  knownUserRoles _ = []

instance (KnownUserRole r, KnownUserRoles rs) => KnownUserRoles ( r ': rs) where
  knownUserRoles _ = (knownUserRole (Proxy :: Proxy r)) : (knownUserRoles (Proxy :: Proxy rs))


newtype AllowedUserRoles (ur :: [UserRole]) = AllowedUser User
  deriving (Eq, Show, Generic)

isAllowedUserRole :: forall (uRoles :: [UserRole]). KnownUserRoles uRoles => User -> Maybe (AllowedUserRoles uRoles)
isAllowedUserRole usr
  | elem (uType usr) allowedUserRoles = Just (AllowedUser usr)
  | otherwise = Nothing
  where allowedUserRoles = knownUserRoles (Proxy :: Proxy uRoles)

instance FromJSON (AllowedUserRoles a)
instance ToJSON (AllowedUserRoles a)

instance ToJWT (AllowedUserRoles a)
instance KnownUserRoles uRoles => FromJWT (AllowedUserRoles uRoles) where
  decodeJWT val = case (decodeJWT val :: Either Text User) of 
    Left x -> Left x
    Right usr -> case (isAllowedUserRole usr :: Maybe (AllowedUserRoles uRoles)) of
      Nothing -> Left $ pack "Not Enough Permission"
      Just u -> Right u

newtype SubId = SubId String
  deriving (Eq, Show, Generic)