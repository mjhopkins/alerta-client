{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}

module Alerta.Types where

import           Alerta.Util

import           Data.Aeson
import qualified Data.Aeson.Encoding       as E
import           Data.Aeson.Types
import           Data.Aeson.TH
import           Data.Default
import           Data.Ix
import           Data.Map                  (Map)
import           Data.String               (IsString(..))
import qualified Data.Text as              T
import           Data.Text                 (Text)
import           Data.Time                 (UTCTime)

import           GHC.Generics

import           Web.HttpApiData

type Resource    = String
type Event       = String
type Service     = String
type Environment = String
type Group       = String
type Origin      = String
type AlertType   = String
type Customer    = String
type Tag         = String
type Email       = String -- TODO actual email type
type Provider    = String -- TODO make into data type
type Limit       = Int    -- actually a positive int
type UUID        = String -- TODO actual UUID type?
type Href        = String -- TODO use URL type for hrefs

data Severity =
    Unknown
  | Trace         -- 8     Grey
  | Debug         -- 7     Purple
  | Informational -- 6     Green
  | Ok            -- 5     Green
  | Normal        -- 5     Green
  | Cleared       -- 5     Green
  | Indeterminate -- 5     Silver
  | Warning       -- 4     Blue
  | Minor         -- 3     Yellow
  | Major         -- 2     Orange
  | Critical      -- 1     Red
  | Security      -- 0     Black
  deriving (Eq, Ord, Bounded, Enum, Ix, Show, Read, Generic)

instance ToHttpApiData Severity where
  toUrlPiece = T.pack . show

instance ToJSONKey Severity where
  toJSONKey = toJSONKeyText (T.toLower . T.pack . show)

instance FromJSONKey Severity where
  fromJSONKey = FromJSONKeyTextParser $ \case
    "security"      -> pure Security
    "critical"      -> pure Critical
    "major"         -> pure Major
    "minor"         -> pure Minor
    "warning"       -> pure Warning
    "indeterminate" -> pure Indeterminate
    "cleared"       -> pure Cleared
    "normal"        -> pure Normal
    "ok"            -> pure Ok
    "informational" -> pure Informational
    "debug"         -> pure Debug
    "trace"         -> pure Trace
    "unknown"       -> pure Unknown
    other           -> fail $ "Could not parse key \"" ++ T.unpack other ++ "\" as Severity"

data Status =     --  Status Code
    OpenStatus    --  1
  | AssignStatus  --  2
  | AckStatus     --  3
  | ClosedStatus  --  4
  | ExpiredStatus --  5
  | UnknownStatus --  9
  deriving (Eq, Ord, Bounded, Enum, Ix, Show, Read, Generic)

instance ToHttpApiData Status where
  toUrlPiece = showTextLowercase

instance ToJSONKey Status where
  toJSONKey = toJSONKeyText (T.toLower . T.pack . show)

instance FromJSONKey Status where
  fromJSONKey = FromJSONKeyTextParser $ \case
    "open"    -> pure OpenStatus
    "assign"  -> pure AssignStatus
    "ack"     -> pure AckStatus
    "closed"  -> pure ClosedStatus
    "expired" -> pure ExpiredStatus
    "unknown" -> pure UnknownStatus
    other     -> fail $ "Could not parse key \"" ++ T.unpack other ++ "\" as Status"

data TrendIndication = NoChange | LessSevere | MoreSevere
  deriving (Eq, Ord, Bounded, Enum, Ix, Show, Generic)

instance FromHttpApiData TrendIndication where
  parseQueryParam = parseBoundedTextData

--------------------------------------------------------------------------------
-- basic response
--------------------------------------------------------------------------------

data Resp = OkResp | ErrorResp { respMessage :: String }
  deriving (Eq, Show, Generic)

--------------------------------------------------------------------------------
-- alerts
--------------------------------------------------------------------------------

data Alert = Alert {
    alertResource    :: String
  , alertEvent       :: Event
  , alertEnvironment :: Maybe String
  , alertSeverity    :: Maybe Severity
  , alertCorrelate   :: Maybe [Event]
  , alertStatus      :: Maybe Status
  , alertService     :: Maybe [Service]
  , alertGroup       :: Maybe Group     -- defaults to "Misc"
  , alertValue       :: Maybe Value     -- defaults to "n/a"
  , alertText        :: Maybe String
  , alertTags        :: Maybe [Tag]
  , alertAttributes  :: Maybe (Map String String)
  -- Attribute keys must not contain "." or "$"
  -- note that key "ip" will be overwritten
  , alertOrigin      :: Maybe Origin    -- defaults to prog/machine ('%s/%s' % (os.path.basename(sys.argv[0]), platform.uname()[1]))
  , alertType        :: Maybe AlertType -- defaults to "exceptionAlert"
  , alertCreateTime  :: Maybe UTCTime   -- defaults to utcnow()
  , alertTimeout     :: Maybe Int       -- in seconds, defaults to 86400 (24 hours)
  , alertRawData     :: Maybe String
  , alertCustomer    :: Maybe Customer
  } deriving (Eq, Show, Generic)

-- | helper for testing
mkAlert :: Resource -> Event -> Service -> Alert
mkAlert r e s = Alert {
    alertResource    = r
  , alertEvent       = e
  , alertEnvironment = Just "Development" -- supposed to be optional, but RejectPolicy plugin requires it to be one of ALLOWED_ENVIRONMENTS
  , alertSeverity    = Nothing
  , alertCorrelate   = Nothing
  , alertStatus      = Nothing
  , alertService     = Just [s] -- supposed to be optional, but RejectPolicy requires it to be set
  , alertGroup       = Nothing
  , alertValue       = Nothing
  , alertText        = Nothing
  , alertTags        = Nothing
  , alertAttributes  = Nothing
  , alertOrigin      = Nothing
  , alertType        = Nothing
  , alertCreateTime  = Nothing
  , alertTimeout     = Nothing
  , alertRawData     = Nothing
  , alertCustomer    = Nothing
  }

data AlertInfo = AlertInfo {
    alertInfoId               :: UUID
  , alertInfoResource         :: Resource
  , alertInfoEvent            :: Event
  , alertInfoEnvironment      :: Environment
  , alertInfoSeverity         :: Severity
  , alertInfoCorrelate        :: [Event]
  , alertInfoStatus           :: Status
  , alertInfoService          :: [Service]
  , alertInfoGroup            :: Group
  , alertInfoValue            :: Value
  , alertInfoText             :: String
  , alertInfoTags             :: [Tag]
  , alertInfoAttributes       :: Map String String -- Attribute keys must not contain "." or "$"
  , alertInfoOrigin           :: Origin
  , alertInfoType             :: AlertType
  , alertInfoCreateTime       :: UTCTime
  , alertInfoTimeout          :: Int
  , alertInfoRawData          :: String
  , alertInfoCustomer         :: Maybe Customer
  , alertInfoDuplicateCount   :: Int
  , alertInfoRepeat           :: Bool
  , alertInfoPreviousSeverity :: Severity
  , alertInfoTrendIndication  :: TrendIndication
  , alertInfoReceiveTime      :: UTCTime
  , alertInfoLastReceiveId    :: UUID
  , alertInfoLastReceiveTime  :: UTCTime
  , alertInfoHistory          :: [HistoryItem]
  , alertInfoHref             :: Href
  } deriving (Eq, Show, Generic)

data HistoryItem = StatusHistoryItem {
    historyItemEvent      :: Event
  , historyItemStatus     :: Status
  , historyItemText       :: String
  , historyItemId         :: UUID
  , historyItemUpdateTime :: UTCTime
  } | SeverityHistoryItem {
    historyItemEvent      :: Event
  , historyItemSeverity   :: Severity
  , historyItemText       :: String
  , historyItemId         :: UUID
  , historyItemUpdateTime :: UTCTime
  , historyItemValue      :: Value
  } deriving (Eq, Show, Generic)

data CreateAlertResp = OkCreateAlertResp {
    okCreateAlertRespId         :: UUID
  , okCreateAlertRespAlert      :: Maybe AlertInfo -- not present if rate limited or in blackout
  , okCreateAlertRespMessage    :: Maybe Text      -- present when rate limited or in blackout
  } | ErrorCreateAlertResp {
    errorCreateAlertRespMessage :: Text
  } deriving (Eq, Show, Generic)

data AlertResp = OkAlertResp {
    okAlertRespAlert      :: AlertInfo
  , okAlertRespTotal      :: Int
  } | ErrorAlertResp {
    errorAlertRespMessage :: Text
  } deriving (Eq, Show, Generic)

newtype Tags = Tags { tags :: [Tag] } deriving (Eq, Show, Generic)

newtype Attributes = Attributes { attributes :: Map String String } deriving (Eq, Show, Generic)

data StatusChange = StatusChange {
    statusChangeStatus :: Status -- docs say not "unknown" but the api allows it (in fact any text is accepted)
  , statusChangeText   :: Maybe String
  } deriving (Eq, Show, Generic)

--------------------------------------------------------------------------------
-- environments
--------------------------------------------------------------------------------

data EnvironmentInfo = EnvironmentInfo {
    environmentInfoCount       :: Int
  , environmentInfoEnvironment :: String
  } deriving (Eq, Show, Generic)

data EnvironmentsResp = OkEnvironmentsResp {
    okEnvironmentsRespMessage      :: Maybe String
  , okEnvironmentsRespTotal        :: Int
  , okEnvironmentsRespEnvironments :: [EnvironmentInfo]
  } | ErrorEnvironmentsResp {
    errorEnvironmentsRespMessage :: String
  } deriving (Eq, Show, Generic)

--------------------------------------------------------------------------------
-- services
--------------------------------------------------------------------------------

data ServiceInfo = ServiceInfo {
    serviceInfoCount       :: Int
  , serviceInfoEnvironment :: String
  , serviceInfoService     :: String
  } deriving (Eq, Show, Generic)

data ServicesResp = OkServicesResp {
    okServicesRespTotal    :: Int
  , okServicesRespServices :: [ServiceInfo]
  , okServicesRespMessage  :: Maybe String
  } | ErrorServicesResp {
    errorServicesRespMessage :: String
  } deriving (Eq, Show, Generic)

--------------------------------------------------------------------------------
-- blackouts
--------------------------------------------------------------------------------

data Blackout = Blackout {
    blackoutEnvironment :: Environment
  , blackoutResource    :: Maybe Resource
  , blackoutService     :: Maybe Service
  , blackoutEvent       :: Maybe Event
  , blackoutGroup       :: Maybe Group
  , blackoutTags        :: Maybe [Tag]
  , blackoutStartTime   :: Maybe UTCTime -- defaults to now
  , blackoutEndTime     :: Maybe UTCTime -- defaults to start + duration
  , blackoutDuration    :: Maybe Int     -- in seconds; can be calculated from start and end, or else defaults to BLACKOUT_DURATION
  } deriving (Eq, Show, Generic)

-- | helper for testing
blackout :: Environment -> Blackout
blackout env = Blackout env Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

data BlackoutInfo = BlackoutInfo {
    blackoutInfoId          :: UUID
  , blackoutInfoPriority    :: Int
  {-
  priority is 1 by default
              2 if resource and not event present
              3 if service
              4 if event and not resource
              5 if group
              6 if resource and event
              7 if tags
  Somewhat bizarrely, the saved blackout only includes an attribute
  {resource,service,event,group,tags}
  if it was used to deduce the priority,
  i.e. a priority 6 blackout will have resource and event attributes,
  but no tags attribute, even if it was supplied when it was created.
  -}
  , blackoutInfoEnvironment :: Environment
  , blackoutInfoResource    :: Maybe Resource
  , blackoutInfoService     :: Maybe [Service]
  , blackoutInfoEvent       :: Maybe Event
  , blackoutInfoGroup       :: Maybe Group
  , blackoutInfoTags        :: Maybe [Tag]
  , blackoutInfoCustomer    :: Maybe Customer
  , blackoutInfoStartTime   :: UTCTime -- defaults to now
  , blackoutInfoEndTime     :: UTCTime -- defaults to start + duration
  , blackoutInfoDuration    :: Int     -- can be calculated from start and end, or else defaults to BLACKOUT_DURATION
  } deriving (Eq, Show, Generic)

data BlackoutStatus = Expired | Pending | Active deriving (Eq, Ord, Bounded, Enum, Ix, Show, Generic)

data ExtendedBlackoutInfo = ExtendedBlackoutInfo {
    extendedBlackoutInfoId          :: UUID
  , extendedBlackoutInfoPriority    :: Int    -- see comment above
  , extendedBlackoutInfoEnvironment :: Environment
  , extendedBlackoutInfoResource    :: Maybe Resource
  , extendedBlackoutInfoService     :: Maybe [Service]
  , extendedBlackoutInfoEvent       :: Maybe Event
  , extendedBlackoutInfoGroup       :: Maybe Group
  , extendedBlackoutInfoTags        :: Maybe [Tag]
  , extendedBlackoutInfoCustomer    :: Maybe Customer
  , extendedBlackoutInfoStartTime   :: UTCTime -- defaults to now
  , extendedBlackoutInfoEndTime     :: UTCTime -- defaults to start + duration
  , extendedBlackoutInfoDuration    :: Int     -- can be calculated from start and end, or else defaults to BLACKOUT_DURATION
  , extendedBlackoutInfoRemaining   :: Int
  , extendedBlackoutInfoStatus      :: BlackoutStatus
  } deriving (Eq, Show, Generic)

data BlackoutResp = OkBlackoutResp {
    okBlackoutRespId       :: UUID
  , okBlackoutRespBlackout :: BlackoutInfo
  } | ErrorBlackoutResp {
    errorBlackoutRespMessage :: String
  } deriving (Eq, Show, Generic)

data BlackoutsResp = OkBlackoutsResp {
    okBlackoutsRespTotal     :: Int
  , okBlackoutsRespBlackouts :: [ExtendedBlackoutInfo]
  , okBlackoutsRespMessage   :: Maybe String
  , okBlackoutsRespTime      :: UTCTime
  } | ErrorBlackoutsResp {
    errorBlackoutsRespMessage :: String
  } deriving (Eq, Show, Generic)


--------------------------------------------------------------------------------
-- heartbeats
--------------------------------------------------------------------------------

data Heartbeat = Heartbeat {
    heartbeatOrigin     :: Maybe Origin -- defaults to prog/nodename
  , heartbeatTags       :: [Tag]
  , heartbeatCreateTime :: Maybe UTCTime
  , heartbeatTimeout    :: Maybe Int    --seconds
  , heartbeatCustomer   :: Maybe String -- if not admin, gets overwritten
  } deriving (Eq, Show, Generic, Default)

data HeartbeatInfo = HeartbeatInfo {
    heartbeatInfoCreateTime  :: UTCTime
  , heartbeatInfoCustomer    :: Maybe Customer
  , heartbeatInfoHref        :: Href
  , heartbeatInfoId          :: UUID
  , heartbeatInfoOrigin      :: Origin
  , heartbeatInfoReceiveTime :: UTCTime
  , heartbeatInfoTags        :: [String]
  , heartbeatInfoTimeout     :: Int
  , heartbeatInfoType        :: String
  } deriving (Eq, Show, Generic)

data CreateHeartbeatResp = OkCreateHeartbeatResp {
    createHeartbeatRespId        :: UUID
  , createHeartbeatRespHeartbeat :: HeartbeatInfo
  } | ErrorCreateHeartbeatResp {
    createHeartbeatRespMessage   :: String
  } deriving (Eq, Show, Generic)

data HeartbeatResp = OkHeartbeatResp {
    heartbeatRespHeartbeat :: HeartbeatInfo
  , heartbeatRespTotal     :: Int
  } | ErrorHeartbeatResp {
    heartbeatRespMessage   :: String
  } deriving (Eq, Show, Generic)

data HeartbeatsResp = OkHeartbeatsResp {
    heartbeatsRespHeartbeats   :: [HeartbeatInfo]
  , heartbeatsRespTime         :: Maybe UTCTime
  , heartbeatsRespTotal        :: Int
  , heartbeatsRespMessage      :: Maybe String
  }| ErrorHeartbeatsResp {
    heartbeatsRespErrorMessage :: String
  } deriving (Eq, Show, Generic)


--------------------------------------------------------------------------------
-- API keys
--------------------------------------------------------------------------------

-- when using admin credentials, user must be present, or associated with account

-- | 40-char UTF8
data ApiKey = ApiKey { unApiKey :: !Text }
  deriving (Eq, Show, Generic)

instance IsString ApiKey where
  fromString = ApiKey . T.pack

instance FromJSON ApiKey where
  parseJSON = withText "API key" (pure . ApiKey)

instance ToJSON ApiKey where
  toJSON (ApiKey k) = String k
  {-# INLINE toJSON #-}

  toEncoding (ApiKey k) = E.text k
  {-# INLINE toEncoding #-}

instance ToHttpApiData ApiKey where
  toUrlPiece (ApiKey k) = k

data CreateApiKey = CreateApiKey {
    createApiKeyUser     :: Maybe Email       -- only read if authorised as admin, defaults to current user
  , createApiKeyCustomer :: Maybe Customer   -- only read if authorised as admin, defaults to current customer
  , createApiKeyType     :: Maybe ApiKeyType -- defaults to read-only
  , createApiKeyText     :: Maybe String     -- defaults to "API Key for $user"
  } deriving (Eq, Show, Generic, Default)

data ApiKeyType = ReadOnly | ReadWrite deriving (Eq, Ord, Bounded, Enum, Ix, Generic)

instance Show ApiKeyType where
  show ReadWrite = "read-write"
  show ReadOnly  = "read-only"

instance ToJSON ApiKeyType where
  toJSON = genericToJSON $ defaultOptions { constructorTagModifier = camelTo2 '-'}

instance FromJSON ApiKeyType where
  parseJSON = genericParseJSON $ defaultOptions { constructorTagModifier = camelTo2 '-'}

data ApiKeyInfo = ApiKeyInfo {
    apiKeyInfoUser         :: Email
  , apiKeyInfoKey          :: ApiKey
  , apiKeyInfoType         :: ApiKeyType
  , apiKeyInfoText         :: String
  , apiKeyInfoExpireTime   :: UTCTime
  , apiKeyInfoCount        :: Int -- number of times used
  , apiKeyInfoLastUsedTime :: Maybe UTCTime
  , apiKeyInfoCustomer     :: Maybe Customer
  } deriving (Eq, Show, Generic)

data CreateApiKeyResp = OkCreateApiKeyResp {
    okCreateApiKeyRespKey        :: ApiKey 
  , okCreateApiKeyRespData       :: ApiKeyInfo
  } | ErrorCreateApiKeyResp {
    errorCreateApiKeyRespMessage :: String
  } deriving (Eq, Show, Generic)

data ApiKeysResp = OkApiKeysResp {
    okApiKeysRespKeys       :: [ApiKeyInfo]
  , okApiKeysRespTotal      :: Int
  , okApiKeysRespTime       :: UTCTime
  , okApiKeysRespMessage    :: Maybe String
  } | ErrorApiKeysResp {
    errorApiKeysRespMessage :: String
  } deriving (Eq, Show, Generic)


$( deriveJSON (toOpts 0 0 def)                    ''Severity             )
$( deriveJSON (toOpts 1 1 def)                    ''Status               )
$( deriveJSON (toOpts 1 1 def)                    ''Alert                )
$( deriveJSON (toOpts 0 0 def { unwrap = False }) ''Tags                 )
$( deriveJSON (toOpts 0 0 def { unwrap = False }) ''Attributes           )
$( deriveJSON (toOpts 3 2 def)                    ''AlertResp            )
$( deriveJSON (toOpts 4 3 def)                    ''CreateAlertResp      )
$( deriveJSON (toOpts 2 2 def)                    ''AlertInfo            )
$( deriveJSON (toOpts 2 2 def)                    ''StatusChange         )
$( deriveJSON (toOpts 2 1 def)                    ''Resp                 )
$( deriveJSON (toOpts 0 0 def)                    ''TrendIndication      )
$( deriveJSON (toOpts 2 2 def { tag = "type" })   ''HistoryItem          )
$( deriveJSON (toOpts 2 2 def)                    ''EnvironmentInfo      )
$( deriveJSON (toOpts 3 2 def)                    ''EnvironmentsResp     )
$( deriveJSON (toOpts 2 2 def)                    ''ServiceInfo          )
$( deriveJSON (toOpts 3 2 def)                    ''ServicesResp         )
$( deriveJSON (toOpts 1 1 def)                    ''Blackout             )
$( deriveJSON (toOpts 2 2 def)                    ''BlackoutInfo         )
$( deriveJSON (toOpts 2 0 def)                    ''BlackoutStatus       )
$( deriveJSON (toOpts 3 3 def)                    ''ExtendedBlackoutInfo )
$( deriveJSON (toOpts 3 2 def)                    ''BlackoutResp         )
$( deriveJSON (toOpts 3 2 def)                    ''BlackoutsResp        )
$( deriveJSON (toOpts 1 1 def)                    ''Heartbeat            )
$( deriveJSON (toOpts 2 2 def)                    ''HeartbeatInfo        )
$( deriveJSON (toOpts 3 3 def)                    ''CreateHeartbeatResp  )
$( deriveJSON (toOpts 2 2 def)                    ''HeartbeatResp        )
$( deriveJSON (toOpts 2 2 def)                    ''HeartbeatsResp       )
$( deriveJSON (toOpts 3 3 def)                    ''CreateApiKey         )
$( deriveJSON (toOpts 3 3 def)                    ''ApiKeyInfo           )
$( deriveJSON (toOpts 5 4 def)                    ''CreateApiKeyResp     )
$( deriveJSON (toOpts 4 3 def)                    ''ApiKeysResp          )
