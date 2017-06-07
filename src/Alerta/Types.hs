{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}

module Alerta.Types
  ( Resource
  , Event
  , Service
  , Environment
  , Group
  , Origin
  , AlertType
  , UserName
  , CustomerName
  , Tag
  , Email
  , Password
  , Provider
  , ShouldReverse
  , Limit
  , PageNo
  , UUID
  , Href
  , QueryString
  , IsRepeat
  , FieldQuery
  , MatchType(..)
  , (=.), (!=), (~.), (!~)
  , Severity(..)
  , Status(..)
  , TrendIndication(..)
  , Resp(..)
  -- * alerts
  , Alert(..)
  , mkAlert
  , AlertInfo(..)
  , AlertAttr(..)
  , QueryAttr(..)
  , HistoryItem(..)
  , ExtendedHistoryItem(..)
  , Tags(..)
  , Attributes(..)
  , StatusChange(..)
  , CreateAlertResp(..)
  , AlertResp(..)
  , AlertsResp(..)
  , AlertCountResp(..)
  , ResourceInfo(..)
  , Top10Info(..)
  , Top10Resp(..)
  , AlertHistoryResp(..)
  -- * environments
  , EnvironmentInfo(..)
  , EnvironmentsResp(..)
  -- * services
  , ServiceInfo(..)
  , ServicesResp(..)
  -- * blackouts
  , Blackout(..)
  , blackout
  , BlackoutInfo(..)
  , BlackoutStatus(..)
  , ExtendedBlackoutInfo(..)
  , BlackoutResp(..)
  , BlackoutsResp(..)
  -- * heartbeats
  , Heartbeat(..)
  , HeartbeatInfo(..)
  , CreateHeartbeatResp(..)
  , HeartbeatResp(..)
  , HeartbeatsResp(..)
  -- * API keys
  , ApiKey(..)
  , CreateApiKey(..)
  , ApiKeyType(..)
  , ApiKeyInfo(..)
  , CreateApiKeyResp(..)
  , ApiKeysResp(..)
  -- * users
  , User(..)
  , user
  , UserUpdate(..)
  , UserAttr(..)
  , IsEmpty(..)
  , emptyUserAttr
  , checkNonempty
  , withUserName
  , withUserLogin
  , withUserPassword
  , withUserProvider
  , withUserText
  , withUserEmailVerified
  , UserInfo(..)
  , RoleType(..)
  , ExtendedUserInfo(..)
  , UserResp(..)
  , UsersResp(..)
  -- * customers
  , Customer(..)
  , CustomerInfo(..)
  , CustomerResp(..)
  , CustomersResp(..)
  ) where

import           Alerta.Util

import           Control.Applicative (empty)

import           Data.Aeson
import qualified Data.Aeson.Encoding as E
import           Data.Aeson.Types
import           Data.Aeson.TH
import           Data.Coerce         (coerce)
import           Data.Default
import           Data.Ix
import           Data.Map            (Map)
import           Data.Monoid         ((<>))
import           Data.String         (IsString(..))
import qualified Data.Text as        T
import           Data.Text           (Text)
import           Data.Time           (UTCTime)

import           GHC.Generics

import           Web.HttpApiData

type Resource      = String
type Event         = String
type Service       = String
type Environment   = String
type Group         = String
type Origin        = String
type AlertType     = String
type UserName      = String
type CustomerName  = String
type Tag           = String
type Email         = String -- TODO actual email type
type Password      = String
type Provider      = String -- TODO make into data type
type ShouldReverse = Bool
type Limit         = Int    -- actually a positive int
type PageNo        = Int    -- actually a positive int
type UUID          = String -- TODO actual UUID type?
type Href          = String -- TODO use URL type for hrefs
type QueryString   = String -- TODO should be JSON or an ADT representing a Mongo query
type IsRepeat      = Bool
-- ^ false if an alert is correlated (in which case alerta appends an item to
-- the history), true for duplicate

-- n.b. regexes are case-insensitive and are not anchored,
-- i.e. no need to write .*regex.*
type FieldQuery = (QueryAttr, String, MatchType, Bool)

data MatchType = Regex | Literal deriving (Eq, Enum, Show, Read, Generic)

(=.), (!=), (~.), (!~) :: QueryAttr -> String -> FieldQuery
k =. v =  (k, Literal, v, True)
k != v =  (k, Literal, v, False)
k ~. v =  (k, Regex,   v, True)
k !~ v =  (k, Regex,   v, False)

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
    alertResource    :: Resource
  , alertEvent       :: Event
  , alertEnvironment :: Maybe Environment
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
  , alertCustomer    :: Maybe CustomerName
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
  , alertInfoEnvironment      :: Environment -- defaults to empty string
  , alertInfoSeverity         :: Maybe Severity
  , alertInfoCorrelate        :: [Event]
  , alertInfoStatus           :: Maybe Status
  , alertInfoService          :: [Service]
  , alertInfoGroup            :: Group  -- defaults to "misc"
  , alertInfoValue            :: Value  -- defaults to "n/a"
  , alertInfoText             :: String -- defaults to ""
  , alertInfoTags             :: [Tag]
  , alertInfoAttributes       :: Map String String -- Attribute keys must not contain "." or "$"
  , alertInfoOrigin           :: Origin    -- defaults to prog/machine
  , alertInfoType             :: AlertType -- defaults to "exceptionAlert"
  , alertInfoCreateTime       :: UTCTime
  , alertInfoTimeout          :: Int
  , alertInfoRawData          :: Maybe String
  , alertInfoCustomer         :: Maybe CustomerName
  , alertInfoDuplicateCount   :: Maybe Int
  , alertInfoRepeat           :: Maybe Bool
  , alertInfoPreviousSeverity :: Maybe Severity
  , alertInfoTrendIndication  :: Maybe TrendIndication
  , alertInfoReceiveTime      :: UTCTime
  , alertInfoLastReceiveId    :: Maybe UUID
  , alertInfoLastReceiveTime  :: UTCTime
  , alertInfoHistory          :: [HistoryItem]
  , alertInfoHref             :: Href
  } deriving (Eq, Show, Generic)

data AlertAttr =
    IdAlertAttr
  | ResourceAlertAttr
  | EventAlertAttr
  | EnvironmentAlertAttr
  | SeverityAlertAttr
  | CorrelateAlertAttr
  | StatusAlertAttr
  | ServiceAlertAttr
  | GroupAlertAttr
  | ValueAlertAttr
  | TextAlertAttr
  | TagsAlertAttr
  | AttributesAlertAttr
  | OriginAlertAttr
  | TypeAlertAttr
  | CreateTimeAlertAttr
  | TimeoutAlertAttr
  | RawDataAlertAttr
  | CustomerAlertAttr
  | DuplicateCountAlertAttr
  | RepeatAlertAttr
  | PreviousSeverityAlertAttr
  | TrendIndicationAlertAttr
  | ReceiveTimeAlertAttr
  | LastReceiveIdAlertAttr
  | LastReceiveTimeAlertAttr
  | HistoryAlertAttr
  | HrefAlertAttr
    deriving (Eq, Enum, Show, Generic)

instance IsString AlertAttr where
   fromString "id"                = IdAlertAttr
   fromString "resource"          = ResourceAlertAttr
   fromString "event"             = EventAlertAttr
   fromString "environment"       = EnvironmentAlertAttr
   fromString "severity"          = SeverityAlertAttr
   fromString "correlate"         = CorrelateAlertAttr
   fromString "status"            = StatusAlertAttr
   fromString "service"           = ServiceAlertAttr
   fromString "group"             = GroupAlertAttr
   fromString "value"             = ValueAlertAttr
   fromString "text"              = TextAlertAttr
   fromString "tags"              = TagsAlertAttr
   fromString "attributes"        = AttributesAlertAttr
   fromString "origin"            = OriginAlertAttr
   fromString "type"              = TypeAlertAttr
   fromString "createTime"        = CreateTimeAlertAttr
   fromString "timeout"           = TimeoutAlertAttr
   fromString "rawData"           = RawDataAlertAttr
   fromString "customer"          = CustomerAlertAttr
   fromString "duplicateCount"    = DuplicateCountAlertAttr
   fromString "repeat"            = RepeatAlertAttr
   fromString "previousSeverity"  = PreviousSeverityAlertAttr
   fromString "trendIndication"   = TrendIndicationAlertAttr
   fromString "receiveTime"       = ReceiveTimeAlertAttr
   fromString "lastReceiveId"     = LastReceiveIdAlertAttr
   fromString "lastReceiveTime"   = LastReceiveTimeAlertAttr
   fromString "history"           = HistoryAlertAttr
   fromString "href"              = HrefAlertAttr
   fromString other = error $ "\"" ++ other ++ "\" is not a valid AlertAttr"

instance ToHttpApiData AlertAttr where
  toQueryParam = T.pack . uncapitalise . onCamelComponents (dropRight 2) . show

-- | No 'id', 'repeat' or 'duplicateCount' as these have special handling
data QueryAttr =
    EventQueryAttr
  | EnvironmentQueryAttr
  | SeverityQueryAttr
  | CorrelateQueryAttr
  | StatusQueryAttr
  | ServiceQueryAttr
  | GroupQueryAttr
  | ValueQueryAttr
  | TextQueryAttr
  | TagsQueryAttr
  | AttributesQueryAttr
  | OriginQueryAttr
  | TypeQueryAttr
  | CreateTimeQueryAttr
  | TimeoutQueryAttr
  | RawDataQueryAttr
  | CustomerQueryAttr
  | RepeatQueryAttr
  | PreviousSeverityQueryAttr
  | TrendIndicationQueryAttr
  | ReceiveTimeQueryAttr
  | LastReceiveIdQueryAttr
  | LastReceiveTimeQueryAttr
  | HistoryQueryAttr
  | HrefQueryAttr
    deriving (Eq, Enum, Show, Generic)

instance IsString QueryAttr where
  fromString "event"            = EventQueryAttr
  fromString "environment"      = EnvironmentQueryAttr
  fromString "severity"         = SeverityQueryAttr
  fromString "correlate"        = CorrelateQueryAttr
  fromString "status"           = StatusQueryAttr
  fromString "service"          = ServiceQueryAttr
  fromString "group"            = GroupQueryAttr
  fromString "value"            = ValueQueryAttr
  fromString "text"             = TextQueryAttr
  fromString "tags"             = TagsQueryAttr
  fromString "attributes"       = AttributesQueryAttr
  fromString "origin"           = OriginQueryAttr
  fromString "type"             = TypeQueryAttr
  fromString "createTime"       = CreateTimeQueryAttr
  fromString "timeout"          = TimeoutQueryAttr
  fromString "rawData"          = RawDataQueryAttr
  fromString "customer"         = CustomerQueryAttr
  fromString "repeat"           = RepeatQueryAttr
  fromString "previousSeverity" = PreviousSeverityQueryAttr
  fromString "trendIndication"  = TrendIndicationQueryAttr
  fromString "receiveTime"      = ReceiveTimeQueryAttr
  fromString "lastReceiveId"    = LastReceiveIdQueryAttr
  fromString "lastReceiveTime"  = LastReceiveTimeQueryAttr
  fromString "history"          = HistoryQueryAttr
  fromString "href"             = HrefQueryAttr
  fromString other = error $ "\"" ++ other ++ "\" is not a valid QueryAttr"

instance ToHttpApiData QueryAttr where
  toQueryParam = T.pack . uncapitalise . onCamelComponents (dropRight 2) . show

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

data ExtendedHistoryItem = StatusExtendedHistoryItem {
    statusExtendedHistoryItemId          :: UUID
  , statusExtendedHistoryItemResource    :: Resource
  , statusExtendedHistoryItemEvent       :: Event
  , statusExtendedHistoryItemEnvironment :: Environment
  , statusExtendedHistoryItemStatus      :: Status
  , statusExtendedHistoryItemService     :: [Service] -- why this is an array I don't know
  , statusExtendedHistoryItemGroup       :: Group
  , statusExtendedHistoryItemText        :: Text
  , statusExtendedHistoryItemTags        :: [Tag]
  , statusExtendedHistoryItemAttributes  :: Map String String
  , statusExtendedHistoryItemOrigin      :: Origin
  , statusExtendedHistoryItemUpdateTime  :: UTCTime
  , statusExtendedHistoryItemCustomer    :: Maybe CustomerName
  } | SeverityExtendedHistoryItem {
    severityExtendedHistoryItemId          :: UUID
  , severityExtendedHistoryItemResource    :: Resource
  , severityExtendedHistoryItemEvent       :: Event
  , severityExtendedHistoryItemEnvironment :: Environment
  , severityExtendedHistoryItemSeverity    :: Severity
  , severityExtendedHistoryItemService     :: [Service] -- why this is an array I don't know
  , severityExtendedHistoryItemGroup       :: Group
  , severityExtendedHistoryItemValue       :: Value
  , severityExtendedHistoryItemText        :: Text
  , severityExtendedHistoryItemTags        :: [Tag]
  , severityExtendedHistoryItemAttributes  :: Map String String
  , severityExtendedHistoryItemOrigin      :: Origin
  , severityExtendedHistoryItemUpdateTime  :: UTCTime
  , severityExtendedHistoryItemCustomer    :: Maybe CustomerName
  } deriving (Eq, Show, Generic)

newtype Tags = Tags { tags :: [Tag] } deriving (Eq, Show, Generic)

newtype Attributes = Attributes { attributes :: Map String String } deriving (Eq, Show, Generic)

data StatusChange = StatusChange {
    statusChangeStatus :: Status -- docs say not "unknown" but the api allows it
    -- (in fact any text is accepted - but our parsing code need to be able
    -- to read it back from the alert history)
  , statusChangeText   :: Maybe String
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

data AlertsResp = OkAlertsResp {
    okAlertsRespAlerts           :: [AlertInfo]
  , okAlertsRespTotal            :: Int
  , okAlertsRespPage             :: PageNo
  , okAlertsRespPageSize         :: Int
  , okAlertsRespPages            :: Int
  , okAlertsRespMore             :: Bool
  , okAlertsRespSeverityCounts   :: Maybe (Map Severity Int)
  , okAlertsRespStatusCounts     :: Maybe (Map Status Int)
  , okAlertsRespLastTime         :: UTCTime
  , okAlertsRespAutoRefresh      :: Bool
  , okAlertsRespMessage          :: Maybe String
  } | ErrorAlertsResp {
    errorAlertsRespMessage       :: String
  } deriving (Eq, Show, Generic)

data AlertCountResp = OkAlertCountResp {
    okAlertCountRespTotal          :: Int
  , okAlertCountRespSeverityCounts :: Int
  , okAlertCountRespStatusCounts   :: Int
  , okAlertCountRespMessage        :: Maybe String
  } | ErrorAlertCountResp {
    errorAlertCountRespMessage     :: String
  } deriving (Eq, Show, Generic)

data ResourceInfo = ResourceInfo {
    resourceInfoId       :: UUID
  , resourceInfoResource :: Resource
  , resourceInfoHref     :: Href
  } deriving (Eq, Show, Generic)

-- | This also has a field corresponding to the "group-by" query parameter used
-- i.e. if you group by origin, then the result will have an "origin" field.
data Top10Info = Top10Info {
    top10InfoCount          :: Int
  , top10InfoDuplicateCount :: Int
  , top10InfoEnvironments   :: [Environment]
  , top10InfoServices       :: [Service]
  , top10InfoResources      :: [ResourceInfo]
  } deriving (Eq, Show, Generic)

data Top10Resp = OkTop10Resp {
    okTop10RespTop10   :: [Top10Info]
  , okTop10RespTotal   :: Int
  , okTop10RespMessage :: Maybe String
  } | ErrorTop10Resp {
    errorTop10RespMessage :: String
  } deriving (Eq, Show, Generic)

data AlertHistoryResp = OkAlertHistoryResp {
    okAlertHistoryRespHistory  :: [ExtendedHistoryItem]
  , okAlertHistoryRespLastTime :: UTCTime
  , okAlertHistoryRespMessage  :: Maybe String
  } | ErrorAlertHistoryResp {
    errorAlertHistoryResp      :: String
  } deriving (Eq, Show, Generic)

--------------------------------------------------------------------------------
-- environments
--------------------------------------------------------------------------------

data EnvironmentInfo = EnvironmentInfo {
    environmentInfoCount       :: Int
  , environmentInfoEnvironment :: Environment
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
  , serviceInfoEnvironment :: Environment
  , serviceInfoService     :: Service
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
  , blackoutInfoCustomer    :: Maybe CustomerName
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
  , extendedBlackoutInfoCustomer    :: Maybe CustomerName
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
    heartbeatOrigin     :: Maybe Origin       -- defaults to prog/nodename
  , heartbeatTags       :: [Tag]
  , heartbeatCreateTime :: Maybe UTCTime
  , heartbeatTimeout    :: Maybe Int          -- seconds
  , heartbeatCustomer   :: Maybe CustomerName -- if not admin, gets overwritten
  } deriving (Eq, Show, Generic, Default)

data HeartbeatInfo = HeartbeatInfo {
    heartbeatInfoCreateTime  :: UTCTime
  , heartbeatInfoCustomer    :: Maybe CustomerName
  , heartbeatInfoHref        :: Href
  , heartbeatInfoId          :: UUID
  , heartbeatInfoOrigin      :: Origin
  , heartbeatInfoReceiveTime :: UTCTime
  , heartbeatInfoTags        :: [Tag]
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
  , createApiKeyCustomer :: Maybe CustomerName   -- only read if authorised as admin, defaults to current customer
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
  , apiKeyInfoCustomer     :: Maybe CustomerName
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

--------------------------------------------------------------------------------
-- users
--------------------------------------------------------------------------------

-- alerta doesn't require login, password but returns a 500 if they are missing
-- yay, cowboy coding
data User = User {
    userName          :: UserName
  , userLogin         :: Email
  , userPassword      :: Password
  , userProvider      :: Maybe Provider
  , userText          :: Maybe String
  , userEmailVerified :: Maybe Bool
  } deriving (Show, Generic)

user :: UserName -> Email -> Password -> User
user name login password = User name login password Nothing Nothing Nothing

-- bugs:
-- * can't update password without also passing provider=basic
--   as alerta checks the update message, not the user
-- * can't set email_verified to false without providing another parameter
data UserUpdate = UserUpdate {
    userUpdateName          :: Maybe UserName
  , userUpdateLogin         :: Maybe Email
  , userUpdatePassword      :: Maybe Password
  , userUpdateProvider      :: Maybe Provider
  , userUpdateText          :: Maybe String
  , userUpdateEmail_verified :: Maybe Bool
  } deriving (Show, Generic, Default)

data IsEmpty = Empty | Nonempty | UnknownIfEmpty

data UserAttr (u :: IsEmpty) = UserAttr {
    userAttrName           :: Maybe UserName
  , userAttrLogin          :: Maybe Email
  , userAttrPassword       :: Maybe Password
  , userAttrProvider       :: Maybe Provider
  , userAttrText           :: Maybe String
  , userAttrEmail_verified :: Maybe Bool
  } deriving (Eq, Show, Generic)

emptyUserAttr :: UserAttr 'Empty
emptyUserAttr = UserAttr Nothing Nothing Nothing Nothing Nothing Nothing

checkNonempty :: UserAttr u -> Either (UserAttr 'Empty) (UserAttr 'Nonempty)
checkNonempty a@(UserAttr Nothing Nothing Nothing Nothing Nothing Nothing) =  Left $ coerce a
checkNonempty a = Right $ coerce a

instance Default (UserAttr 'Empty) where
  def = emptyUserAttr

-- TODO omitNothingFields doesn't work
instance ToJSON (UserAttr 'Nonempty) where
  toJSON = genericToJSON $ defaultOptions { omitNothingFields = True }

  toEncoding (UserAttr n l pw pr t ev) =
    pairs $
     "name"           .= n  <>
     "login"          .= l  <>
     "password"       .= pw <>
     "provider"       .= pr <>
     "text"           .= t  <>
     "email_verified" .= ev

instance FromJSON (UserAttr 'UnknownIfEmpty) where
  parseJSON (Object v) =
    UserAttr <$>
      v .:? "name" <*>
      v .:? "login" <*>
      v .:? "password" <*>
      v .:? "provider" <*>
      v .:? "text" <*>
      v .:? "email_verified"
  parseJSON _ = empty

-- TODO lenses
withUserName          :: UserAttr u -> UserName -> UserAttr 'Nonempty
withUserLogin         :: UserAttr u -> Email    -> UserAttr 'Nonempty
withUserPassword      :: UserAttr u -> Password -> UserAttr 'Nonempty
withUserProvider      :: UserAttr u -> Provider -> UserAttr 'Nonempty
withUserText          :: UserAttr u -> String   -> UserAttr 'Nonempty
withUserEmailVerified :: UserAttr u -> Bool     -> UserAttr u
withUserName          u s = u { userAttrName = Just s }
withUserLogin         u s = u { userAttrLogin = Just s }
-- sets the provider to basic to work around the bug mentioned above
withUserPassword      u s = u { userAttrPassword = Just s, userAttrProvider = Just "basic" }
withUserProvider      u s = u { userAttrProvider = Just s }
withUserText          u s = u { userAttrText = Just s }
withUserEmailVerified u b = u { userAttrEmail_verified = Just b }

data UserInfo = UserInfo {
    userInfoCreateTime     :: UTCTime
  , userInfoId             :: UUID
  , userInfoName           :: UserName
  , userInfoProvider       :: Provider
  , userInfoLogin          :: Email
  , userInfoText           :: String
  , userInfoEmail_verified :: Bool
  } deriving (Show, Generic)

data RoleType = UserRoleType | AdminRoleType
 deriving (Eq, Ord, Bounded, Enum, Ix, Show, Generic)

data ExtendedUserInfo = ExtendedUserInfo {
    extendedUserInfoCreateTime     :: UTCTime
  , extendedUserInfoId             :: UUID
  , extendedUserInfoName           :: UserName
  , extendedUserInfoLogin          :: Email
  , extendedUserInfoProvider       :: Provider
  , extendedUserInfoRole           :: RoleType
  , extendedUserInfoText           :: String
  , extendedUserInfoEmail_verified :: Bool
  } deriving (Show, Generic)

data UserResp = OkUserResp {
    okUserRespId   :: UUID
  , okUserRespUser :: UserInfo
  } | ErrorUserResp {
    errorUserRespMessage :: String
  } deriving (Show, Generic)

data UsersResp = OkUsersResp {
    okUsersRespUsers   :: [ExtendedUserInfo]
  , okUsersRespTotal   :: Int
  , okUsersRespDomains :: [String]       -- allowed email domains
  , okUsersRespGroups  :: [String]       -- allowed Gitlab groups
  , okUsersRespOrgs    :: [String]       -- allowed Github orgs
  , okUsersRespRoles   :: Maybe [String] -- allowed Keycloud roles
  , okUsersRespTime    :: UTCTime
  , okUsersRespMessage :: Maybe String
  } | ErrorUsersResp {
    errorUsersResp :: String
  } deriving (Show, Generic)

--------------------------------------------------------------------------------
-- customers
--------------------------------------------------------------------------------

data Customer = Customer {
    customerCustomer :: CustomerName
  , customerMatch    :: String -- regex, apparently
  } deriving (Show, Generic)

data CustomerInfo = CustomerInfo {
    customerInfoId       :: UUID
  , customerInfoCustomer :: CustomerName
  , customerInfoMatch    :: String -- regex, apparently
  } deriving (Show, Generic)

data CustomerResp = OkCustomerResp {
    okCustomerRespId         :: UUID
  , okCustomerRespCustomer   :: CustomerInfo
  } | ErrorCustomerResp {
    errorCustomerRespMessage :: String
  } deriving (Show, Generic)

data CustomersResp = OkCustomersResp {
    okCustomersRespCustomers :: [CustomerInfo]
  , okCustomersRespTotal     :: Int
  , okCustomersRespMessage   :: Maybe String
  , okCustomersRespTime      :: UTCTime
  } | ErrorCustomersResp {
    errorCustomersMessage    :: String
  } deriving (Show, Generic)

$( deriveJSON (toOpts 0 0 def)                    ''Severity             )
$( deriveJSON (toOpts 1 1 def)                    ''Status               )
$( deriveJSON (toOpts 1 1 def)                    ''Alert                )
$( deriveJSON (toOpts 0 0 def { unwrap = False }) ''Tags                 )
$( deriveJSON (toOpts 0 0 def { unwrap = False }) ''Attributes           )
$( deriveJSON (toOpts 2 2 def)                    ''AlertAttr            )
$( deriveJSON (toOpts 2 2 def)                    ''AlertInfo            )
$( deriveJSON (toOpts 4 3 def)                    ''CreateAlertResp      )
$( deriveJSON (toOpts 3 2 def)                    ''AlertResp            )
$( deriveJSON (toOpts 3 2 def)                    ''AlertsResp           )
$( deriveJSON (toOpts 2 2 def)                    ''ResourceInfo         )
$( deriveJSON (toOpts 2 2 def)                    ''Top10Info            )
$( deriveJSON (toOpts 3 2 def)                    ''Top10Resp            )
$( deriveJSON (toOpts 4 3 def)                    ''AlertCountResp       )
$( deriveJSON (toOpts 4 3 def)                    ''AlertHistoryResp     )
$( deriveJSON (toOpts 2 2 def)                    ''StatusChange         )
$( deriveJSON (toOpts 2 1 def)                    ''Resp                 )
$( deriveJSON (toOpts 0 0 def)                    ''TrendIndication      )
$( deriveJSON (toOpts 2 2 def { tag = "type" })   ''HistoryItem          )
$( deriveJSON (toOpts 4 3 def { tag = "type" })   ''ExtendedHistoryItem  )
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
$( deriveJSON (toOpts 3 2 def)                    ''RoleType             )
$( deriveJSON (toOpts 1 1 def)                    ''User                 )
$( deriveJSON (toOpts 2 2 def)                    ''UserInfo             )
$( deriveJSON (toOpts 3 3 def)                    ''ExtendedUserInfo     )
$( deriveJSON (toOpts 3 2 def)                    ''UserResp             )
$( deriveJSON (toOpts 3 2 def)                    ''UsersResp            )
$( deriveJSON (toOpts 1 1 def)                    ''Customer             )
$( deriveJSON (toOpts 2 2 def)                    ''CustomerInfo         )
$( deriveJSON (toOpts 3 2 def)                    ''CustomerResp         )
$( deriveJSON (toOpts 3 2 def)                    ''CustomersResp        )
