{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

--------------------------------------------------------------------------------
-- |
-- Module: Alerta.Types
--
-- Types used by the alerta bindings.
-- This includes request and return parameter types, as well as type synonyms used
-- to help document the bindings.
--
-- A record whose name ends in \"Resp\" indicates a response from the server.
-- A record whose name ends in \"Info\" indicates data contained within
-- responses from the server.
--------------------------------------------------------------------------------
module Alerta.Types
  (
  -- ** General domain terms
    Resource
  , Event
  , Service
  , Environment
  , Group
  , Value
  , Origin
  , AlertType
  , UserName
  , CustomerName
  , Tag
  , Email
  , Password
  , Provider
  , UUID
  , Href
  -- ** Query and sorting options
  , ShouldReverse
  , Limit
  , PageNo
  , QueryString
  , IsRepeat
  -- ** Field queries
  , FieldQuery
  , MatchType(..)
  , (=.), (!=), (~.), (!~)
  -- ** Generic response
  , Resp(..)
  -- ** Alerts
  , Severity(..)
  , Status(..)
  , TrendIndication(..)
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
  -- ** Environments
  , EnvironmentInfo(..)
  , EnvironmentsResp(..)
  -- ** Services
  , ServiceInfo(..)
  , ServicesResp(..)
  -- ** Blackouts
  , Blackout(..)
  , blackout
  , BlackoutInfo(..)
  , BlackoutStatus(..)
  , ExtendedBlackoutInfo(..)
  , BlackoutResp(..)
  , BlackoutsResp(..)
  -- ** Heartbeats
  , Heartbeat(..)
  , HeartbeatInfo(..)
  , CreateHeartbeatResp(..)
  , HeartbeatResp(..)
  , HeartbeatsResp(..)
  -- ** API keys
  , ApiKey(..)
  , CreateApiKey(..)
  , ApiKeyType(..)
  , ApiKeyInfo(..)
  , CreateApiKeyResp(..)
  , ApiKeysResp(..)
  -- ** Users
  , User(..)
  , user
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
  -- ** Customers
  , Customer(..)
  , CustomerInfo(..)
  , CustomerResp(..)
  , CustomersResp(..)

  , Response(..)
  , UserResp'(..)
  ) where

import           Alerta.Util

import           Control.Applicative (empty)

import           Data.Aeson          hiding (Value)
import qualified Data.Aeson          as Aeson
import qualified Data.Aeson.Encoding as E
import           Data.Aeson.TH
import           Data.Aeson.Types    hiding (Value)
import           Data.Coerce         (coerce)
import           Data.Default
import qualified Data.HashMap.Strict as HM
import           Data.Ix
import           Data.Map            (Map)
import           Data.Monoid         ((<>))
import           Data.String         (IsString (..))
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Time           (UTCTime)

import           GHC.Generics

import           Web.HttpApiData

type Resource      = Text
type Event         = Text
type Service       = Text
type Environment   = Text
type Group         = Text
type Value         = Text
type Origin        = Text
type AlertType     = Text
type UserName      = Text
type CustomerName  = Text
type Tag           = Text
type Email         = Text -- TODO actual email type
type Password      = Text
type Provider      = Text -- TODO make into data type
type ShouldReverse = Bool   -- ^ whether to reverse the order of a sort
type Limit         = Int    -- ^ maximum number of results to return (actually a positive int)
type PageNo        = Int    -- ^ what page of the results to return (actually a positive int)
type UUID          = Text -- TODO actual UUID type?
type Href          = Text -- TODO use URL type for hrefs
-- | This is a JSON document describing a Mongo query,
-- see http://docs.mongodb.org/manual/reference/operator/query/
type QueryString   = Text -- TODO should be JSON or an ADT representing a Mongo query
type IsRepeat      = Bool
-- ^ true for duplicate, false if an alert is correlated (in which case alerta appends an item to
-- the history)

--------------------------------------------------------------------------------
-- Field queries
--------------------------------------------------------------------------------

type FieldQuery = (QueryAttr, Text, MatchType, Bool)

-- | Matches can be either literal or regular expressions.
-- n.b. regexes are case-insensitive and are not anchored,
-- i.e. no need to write .*regex.*
data MatchType = Regex | Literal deriving (Eq, Enum, Show, Read, Generic)

-- | Convenient syntax for the four types of field queries
-- viz. literal, negated literal, regex, negated regex.
(=.), (!=), (~.), (!~) :: QueryAttr -> Text -> FieldQuery
k =. v =  (k, v, Literal, True)
k != v =  (k, v, Literal, False)
k ~. v =  (k, v, Regex,   True)
k !~ v =  (k, v, Regex,   False)

-- | These are the valid keys for use in field queries.
--
-- NB no 'id', 'repeat' or 'duplicateCount' as these have special handling.
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

-- | Alert severity
data Severity =
    Unknown
  | Trace         -- ^ 8 (grey)
  | Debug         -- ^ 7 (purple)
  | Informational -- ^ 6 (green)
  | Ok            -- ^ 5 (green)
  | Normal        -- ^ 5 (green)
  | Cleared       -- ^ 5 (green)
  | Indeterminate -- ^ 5 (silver)
  | Warning       -- ^ 4 (blue)
  | Minor         -- ^ 3 (yellow)
  | Major         -- ^ 2 (orange)
  | Critical      -- ^ 1 (red)
  | Security      -- ^ 0 (black)
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

-- | Status of an alert.
data Status =     --  Status Code
    OpenStatus    -- ^ status code 1
  | AssignStatus  -- ^ status code 2
  | AckStatus     -- ^ status code 3
  | ClosedStatus  -- ^ status code 4
  | ExpiredStatus -- ^ status code 5
  | UnknownStatus -- ^ status code 9
  deriving (Eq, Ord, Bounded, Enum, Ix, Show, Read, Generic)

instance ToHttpApiData Status where
  toUrlPiece = showTextLowercase

instance ToJSONKey Status where
  -- toJSONKey = toJSONKeyText (T.toLower . T.pack . show)
  toJSONKey = toJSONKeyText $ \case
    OpenStatus    -> "open"    
    AssignStatus  -> "assign"      
    AckStatus     -> "ack"   
    ClosedStatus  -> "closed"      
    ExpiredStatus -> "expired"       
    UnknownStatus -> "unknown"       

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
-- Basic response
--------------------------------------------------------------------------------

-- | This type is used for basic responses that have no content beyond whether they succeeded or
-- failed with an error message.
data Resp = OkResp | ErrorResp { respMessage :: Text }
  deriving (Eq, Show, Generic)

--------------------------------------------------------------------------------
-- Alerts
--------------------------------------------------------------------------------

-- | Data required to create (post) an alert.
data Alert = Alert
  { alertResource    :: Resource
  , alertEvent       :: Event
  , alertEnvironment :: Maybe Environment
  , alertSeverity    :: Maybe Severity
  , alertCorrelate   :: Maybe [Event]
  , alertStatus      :: Maybe Status
  , alertService     :: Maybe [Service]
  , alertGroup       :: Maybe Group     -- ^ defaults to @Misc@
  , alertValue       :: Maybe Value     -- ^ defaults to @n/a@
  , alertText        :: Maybe Text
  , alertTags        :: Maybe [Tag]
  , alertAttributes  :: Maybe (Map Text Text) --TODO Attributes
  -- Attribute keys must not contain "." or "$"
  -- note that key "ip" will be overwritten
  , alertOrigin      :: Maybe Origin    -- ^ defaults to prog\/machine ('%s\/%s' % (os.path.basename(sys.argv[0]), platform.uname()[1]))
  , alertType        :: Maybe AlertType -- ^ defaults to @exceptionAlert@
  , alertCreateTime  :: Maybe UTCTime   -- ^ defaults to @utcnow()@
  , alertTimeout     :: Maybe Int       -- ^ in seconds; defaults to 86400 (24 hours)
  , alertRawData     :: Maybe Text
  , alertCustomer    :: Maybe CustomerName
  } deriving (Eq, Show, Generic)

-- | Create an alert with just the mandatory fields.
mkAlert :: Resource -> Event -> Service -> Alert
mkAlert r e s = Alert
  { alertResource    = r
  , alertEvent       = e
  -- supposed to be optional, but RejectPolicy plugin requires it to be one of ALLOWED_ENVIRONMENTS
  , alertEnvironment = Just "Development"
  , alertSeverity    = Nothing
  , alertCorrelate   = Nothing
  , alertStatus      = Nothing
  -- supposed to be optional, but RejectPolicy requires it to be set
  , alertService     = Just [s]
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

-- | Data returned from the server about an alert
data AlertInfo = AlertInfo
  { alertInfoId               :: UUID
  , alertInfoResource         :: Resource
  , alertInfoEvent            :: Event
  , alertInfoEnvironment      :: Environment -- ^ defaults to empty string
  , alertInfoSeverity         :: Maybe Severity
  , alertInfoCorrelate        :: [Event]
  , alertInfoStatus           :: Maybe Status
  , alertInfoService          :: [Service]
  , alertInfoGroup            :: Group  -- ^ defaults to @misc@
  , alertInfoValue            :: Value  -- ^ defaults to @n/a@
  , alertInfoText             :: Text -- ^ defaults to empty string
  , alertInfoTags             :: [Tag]
  , alertInfoAttributes       :: Map Text Text -- ^ Attribute keys must not contain "." or "$"
  , alertInfoOrigin           :: Origin            -- ^ defaults to prog\/machine
  , alertInfoType             :: AlertType         -- ^ defaults to @exceptionAlert@
  , alertInfoCreateTime       :: UTCTime
  , alertInfoTimeout          :: Int
  , alertInfoRawData          :: Maybe Text
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

-- | Alert attributes, used for sorting, grouping and for field-based queries
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

data HistoryItem = StatusHistoryItem
  { historyItemEvent      :: Event
  , historyItemStatus     :: Status
  , historyItemText       :: Text
  , historyItemId         :: UUID
  , historyItemUpdateTime :: UTCTime
  } | SeverityHistoryItem {
    historyItemEvent      :: Event
  , historyItemSeverity   :: Severity
  , historyItemText       :: Text
  , historyItemId         :: UUID
  , historyItemUpdateTime :: UTCTime
  , historyItemValue      :: Value
  } deriving (Eq, Show, Generic)

-- | When performing an alert history query an enriched version of the alert history is returned with extra fields.
data ExtendedHistoryItem = StatusExtendedHistoryItem
  { statusExtendedHistoryItemId          :: UUID
  , statusExtendedHistoryItemResource    :: Resource
  , statusExtendedHistoryItemEvent       :: Event
  , statusExtendedHistoryItemEnvironment :: Environment
  , statusExtendedHistoryItemStatus      :: Status
  , statusExtendedHistoryItemService     :: [Service] -- ^ why this is an array I do not know
  , statusExtendedHistoryItemGroup       :: Group
  , statusExtendedHistoryItemText        :: Text
  , statusExtendedHistoryItemTags        :: [Tag]
  , statusExtendedHistoryItemAttributes  :: Map Text Text
  , statusExtendedHistoryItemOrigin      :: Origin
  , statusExtendedHistoryItemUpdateTime  :: UTCTime
  , statusExtendedHistoryItemCustomer    :: Maybe CustomerName
  } | SeverityExtendedHistoryItem
  { severityExtendedHistoryItemId          :: UUID
  , severityExtendedHistoryItemResource    :: Resource
  , severityExtendedHistoryItemEvent       :: Event
  , severityExtendedHistoryItemEnvironment :: Environment
  , severityExtendedHistoryItemSeverity    :: Severity
  , severityExtendedHistoryItemService     :: [Service] -- ^ why this is an array I do not know
  , severityExtendedHistoryItemGroup       :: Group
  , severityExtendedHistoryItemValue       :: Value
  , severityExtendedHistoryItemText        :: Text
  , severityExtendedHistoryItemTags        :: [Tag]
  , severityExtendedHistoryItemAttributes  :: Map Text Text
  , severityExtendedHistoryItemOrigin      :: Origin
  , severityExtendedHistoryItemUpdateTime  :: UTCTime
  , severityExtendedHistoryItemCustomer    :: Maybe CustomerName
  } deriving (Eq, Show, Generic)

newtype Tags = Tags { tags :: [Tag] } deriving (Eq, Show, Generic)

-- | Attributes are key-value pairs that can be attached to an alert.
newtype Attributes = Attributes { attributes :: Map Text Text } deriving (Eq, Show, Generic)

data StatusChange = StatusChange
  { statusChangeStatus :: Status -- docs say not "unknown" but the api allows it
    -- (in fact any text is accepted - but our parsing code need to be able
    -- to read it back from the alert history)
  , statusChangeText   :: Maybe Text
  } deriving (Eq, Show, Generic)

data CreateAlertResp = OkCreateAlertResp
  { okCreateAlertRespId      :: UUID
  , okCreateAlertRespAlert   :: Maybe AlertInfo -- ^ not present if rate limited or in blackout
  , okCreateAlertRespMessage :: Maybe Text      -- ^ present when rate limited or in blackout
  } | ErrorCreateAlertResp {
    errorCreateAlertRespMessage :: Text
  } deriving (Eq, Show, Generic)

data AlertResp = OkAlertResp
  { okAlertRespAlert :: AlertInfo
  , okAlertRespTotal :: Int
  } | ErrorAlertResp {
    errorAlertRespMessage :: Text
  } deriving (Eq, Show, Generic)

data AlertsResp = OkAlertsResp
  { okAlertsRespAlerts         :: [AlertInfo]
  , okAlertsRespTotal          :: Int
  , okAlertsRespPage           :: PageNo
  , okAlertsRespPageSize       :: Int
  , okAlertsRespPages          :: Int
  , okAlertsRespMore           :: Bool
  , okAlertsRespSeverityCounts :: Maybe (Map Severity Int)
  , okAlertsRespStatusCounts   :: Maybe (Map Status Int)
  , okAlertsRespLastTime       :: UTCTime
  , okAlertsRespAutoRefresh    :: Bool
  , okAlertsRespMessage        :: Maybe Text
  } | ErrorAlertsResp
  { errorAlertsRespMessage       :: Text
  } deriving (Eq, Show, Generic)

data AlertCountResp = OkAlertCountResp
  { okAlertCountRespTotal          :: Int
  , okAlertCountRespSeverityCounts :: Int
  , okAlertCountRespStatusCounts   :: Int
  , okAlertCountRespMessage        :: Maybe Text
  } | ErrorAlertCountResp
  { errorAlertCountRespMessage     :: Text
  } deriving (Eq, Show, Generic)

data ResourceInfo = ResourceInfo
  { resourceInfoId       :: UUID
  , resourceInfoResource :: Resource
  , resourceInfoHref     :: Href
  } deriving (Eq, Show, Generic)

-- | This also has a field corresponding to the "group-by" query parameter used
-- i.e. if you group by origin, then the result will have an "origin" field.
--
-- This dependently-typed feature is not currently captured in the Haskell types.
data Top10Info = Top10Info
  { top10InfoCount          :: Int
  , top10InfoDuplicateCount :: Int
  , top10InfoEnvironments   :: [Environment]
  , top10InfoServices       :: [Service]
  , top10InfoResources      :: [ResourceInfo]
  } deriving (Eq, Show, Generic)

data Top10Resp = OkTop10Resp
  { okTop10RespTop10   :: [Top10Info]
  , okTop10RespTotal   :: Int
  , okTop10RespMessage :: Maybe Text
  } | ErrorTop10Resp
  { errorTop10RespMessage :: Text
  } deriving (Eq, Show, Generic)

data AlertHistoryResp = OkAlertHistoryResp
  { okAlertHistoryRespHistory  :: [ExtendedHistoryItem]
  , okAlertHistoryRespLastTime :: UTCTime
  , okAlertHistoryRespMessage  :: Maybe Text
  } | ErrorAlertHistoryResp
  { errorAlertHistoryResp      :: Text
  } deriving (Eq, Show, Generic)

--------------------------------------------------------------------------------
-- Environments
--------------------------------------------------------------------------------

data EnvironmentInfo = EnvironmentInfo
  { environmentInfoCount       :: Int
  , environmentInfoEnvironment :: Environment
  } deriving (Eq, Show, Generic)

data EnvironmentsResp = OkEnvironmentsResp
  { okEnvironmentsRespMessage      :: Maybe Text
  , okEnvironmentsRespTotal        :: Int
  , okEnvironmentsRespEnvironments :: [EnvironmentInfo]
  } | ErrorEnvironmentsResp
  { errorEnvironmentsRespMessage :: Text
  } deriving (Eq, Show, Generic)

--------------------------------------------------------------------------------
-- Services
--------------------------------------------------------------------------------

data ServiceInfo = ServiceInfo
  { serviceInfoCount       :: Int
  , serviceInfoEnvironment :: Environment
  , serviceInfoService     :: Service
  } deriving (Eq, Show, Generic)

data ServicesResp = OkServicesResp
  { okServicesRespTotal    :: Int
  , okServicesRespServices :: [ServiceInfo]
  , okServicesRespMessage  :: Maybe Text
  } | ErrorServicesResp
  { errorServicesRespMessage :: Text
  } deriving (Eq, Show, Generic)

--------------------------------------------------------------------------------
-- Blackouts
--------------------------------------------------------------------------------

data Blackout = Blackout
  { blackoutEnvironment :: Environment
  , blackoutResource    :: Maybe Resource
  , blackoutService     :: Maybe Service
  , blackoutEvent       :: Maybe Event
  , blackoutGroup       :: Maybe Group
  , blackoutTags        :: Maybe [Tag]
  , blackoutStartTime   :: Maybe UTCTime -- ^ defaults to now
  , blackoutEndTime     :: Maybe UTCTime -- ^ defaults to start + duration
  , blackoutDuration    :: Maybe Int     -- ^ in seconds; can be calculated from start and end, or else defaults to BLACKOUT_DURATION
  } deriving (Eq, Show, Generic)

-- | Create a blackout with only the mandatory fields
blackout :: Environment -> Blackout
blackout env = Blackout env Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | A note on blackout priorities:
--
-- Priority is
--
--   1. by default
--
--   2. if resource and not event present
--
--   3. if service present
--
--   4. if event and not resource
--
--   5. if group present
--
--   6. if resource and event present
--
--   7. if tags present
--
-- Somewhat bizarrely, the saved blackout only includes an attribute
--
--   {resource,service,event,group,tags}
--
-- if it was used to deduce the priority,
-- i.e. a priority 6 blackout will have resource and event attributes,
-- but no tags attribute, even if it was supplied when it was created.
data BlackoutInfo = BlackoutInfo
  { blackoutInfoId          :: UUID
  , blackoutInfoPriority    :: Int
  , blackoutInfoEnvironment :: Environment
  , blackoutInfoResource    :: Maybe Resource
  , blackoutInfoService     :: Maybe [Service]
  , blackoutInfoEvent       :: Maybe Event
  , blackoutInfoGroup       :: Maybe Group
  , blackoutInfoTags        :: Maybe [Tag]
  , blackoutInfoCustomer    :: Maybe CustomerName
  , blackoutInfoStartTime   :: UTCTime -- ^ defaults to now
  , blackoutInfoEndTime     :: UTCTime -- ^ defaults to start + duration
  , blackoutInfoDuration    :: Int     -- ^ can be calculated from start and end, or else defaults to BLACKOUT_DURATION
  } deriving (Eq, Show, Generic)

data BlackoutStatus = Expired | Pending | Active deriving (Eq, Ord, Bounded, Enum, Ix, Show, Generic)

data ExtendedBlackoutInfo = ExtendedBlackoutInfo
  { extendedBlackoutInfoId          :: UUID
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

data BlackoutResp = OkBlackoutResp
  { okBlackoutRespId       :: UUID
  , okBlackoutRespBlackout :: BlackoutInfo
  } | ErrorBlackoutResp {
    errorBlackoutRespMessage :: Text
  } deriving (Eq, Show, Generic)

data BlackoutsResp = OkBlackoutsResp
  { okBlackoutsRespTotal     :: Int
  , okBlackoutsRespBlackouts :: [ExtendedBlackoutInfo]
  , okBlackoutsRespMessage   :: Maybe Text
  , okBlackoutsRespTime      :: UTCTime
  } | ErrorBlackoutsResp {
    errorBlackoutsRespMessage :: Text
  } deriving (Eq, Show, Generic)


--------------------------------------------------------------------------------
-- Heartbeats
--------------------------------------------------------------------------------

-- | Data needed to create a heartbeat
data Heartbeat = Heartbeat
  { heartbeatOrigin     :: Maybe Origin       -- defaults to prog/nodename
  , heartbeatTags       :: [Tag]
  , heartbeatCreateTime :: Maybe UTCTime
  , heartbeatTimeout    :: Maybe Int          -- seconds
  , heartbeatCustomer   :: Maybe CustomerName -- if not admin, gets overwritten
  } deriving (Eq, Show, Generic, Default)

-- | Information returned from the server about a heartbeat
data HeartbeatInfo = HeartbeatInfo
  { heartbeatInfoCreateTime  :: UTCTime
  , heartbeatInfoCustomer    :: Maybe CustomerName
  , heartbeatInfoHref        :: Href
  , heartbeatInfoId          :: UUID
  , heartbeatInfoOrigin      :: Origin
  , heartbeatInfoReceiveTime :: UTCTime
  , heartbeatInfoTags        :: [Tag]
  , heartbeatInfoTimeout     :: Int
  , heartbeatInfoType        :: Text
  } deriving (Eq, Show, Generic)

data CreateHeartbeatResp = OkCreateHeartbeatResp
  { createHeartbeatRespId        :: UUID
  , createHeartbeatRespHeartbeat :: HeartbeatInfo
  } | ErrorCreateHeartbeatResp
  { createHeartbeatRespMessage   :: Text
  } deriving (Eq, Show, Generic)

data HeartbeatResp = OkHeartbeatResp
  { heartbeatRespHeartbeat :: HeartbeatInfo
  , heartbeatRespTotal     :: Int
  } | ErrorHeartbeatResp
  { heartbeatRespMessage   :: Text
  } deriving (Eq, Show, Generic)

data HeartbeatsResp = OkHeartbeatsResp
  { heartbeatsRespHeartbeats :: [HeartbeatInfo]
  , heartbeatsRespTime       :: Maybe UTCTime
  , heartbeatsRespTotal      :: Int
  , heartbeatsRespMessage    :: Maybe Text
  } | ErrorHeartbeatsResp
  { heartbeatsRespErrorMessage :: Text
  } deriving (Eq, Show, Generic)


--------------------------------------------------------------------------------
-- API keys
--------------------------------------------------------------------------------

-- when using admin credentials, user must be present, or associated with account

-- | 40-char UTF8

-- TODO smart constructor
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

-- | Data needed to create an API key
data CreateApiKey = CreateApiKey
  { createApiKeyUser     :: Maybe Email         -- ^ only read if authorised as admin, defaults to current user
  , createApiKeyCustomer :: Maybe CustomerName  -- ^ only read if authorised as admin, defaults to current customer
  , createApiKeyType     :: Maybe ApiKeyType    -- ^ defaults to read-only
  , createApiKeyText     :: Maybe Text        -- ^ defaults to "API Key for $user"
  } deriving (Eq, Show, Generic, Default)

data ApiKeyType = ReadOnly | ReadWrite deriving (Eq, Ord, Bounded, Enum, Ix, Generic)

instance Show ApiKeyType where
  show ReadWrite = "read-write"
  show ReadOnly  = "read-only"

instance ToJSON ApiKeyType where
  toJSON = genericToJSON $ defaultOptions { constructorTagModifier = camelTo2 '-'}

instance FromJSON ApiKeyType where
  parseJSON = genericParseJSON $ defaultOptions { constructorTagModifier = camelTo2 '-'}

-- | Information returned from the server about an API key
data ApiKeyInfo = ApiKeyInfo
  { apiKeyInfoUser         :: Email
  , apiKeyInfoKey          :: ApiKey
  , apiKeyInfoType         :: ApiKeyType
  , apiKeyInfoText         :: Text
  , apiKeyInfoExpireTime   :: UTCTime
  , apiKeyInfoCount        :: Int -- number of times used
  , apiKeyInfoLastUsedTime :: Maybe UTCTime
  , apiKeyInfoCustomer     :: Maybe CustomerName
  } deriving (Eq, Show, Generic)

data CreateApiKeyResp = OkCreateApiKeyResp
  { okCreateApiKeyRespKey  :: ApiKey
  , okCreateApiKeyRespData :: ApiKeyInfo
  } | ErrorCreateApiKeyResp
  { errorCreateApiKeyRespMessage :: Text
  } deriving (Eq, Show, Generic)

data ApiKeysResp = OkApiKeysResp
  { okApiKeysRespKeys    :: [ApiKeyInfo]
  , okApiKeysRespTotal   :: Int
  , okApiKeysRespTime    :: UTCTime
  , okApiKeysRespMessage :: Maybe Text
  } | ErrorApiKeysResp
  { errorApiKeysRespMessage :: Text
  } deriving (Eq, Show, Generic)

--------------------------------------------------------------------------------
-- Users
--------------------------------------------------------------------------------

-- alerta doesn't require login, password but returns a 500 if they are missing
-- yay, cowboy coding

-- Data needed to create a user
data User = User
  { userName          :: UserName
  , userLogin         :: Email
  , userPassword      :: Password
  , userProvider      :: Maybe Provider
  , userText          :: Maybe Text
  , userEmailVerified :: Maybe Bool
  } deriving (Eq, Show, Generic)

-- | Create a user with just the mandatory fields.
user :: UserName -> Email -> Password -> User
user name login password = User name login password Nothing Nothing Nothing

data IsEmpty = Empty | Nonempty | UnknownIfEmpty

-- | User attributes, used in updating a user.
-- It's an error to update a user with all attributes missing.
--
-- We track whether at least one attribute has been set with a phantom type.
--
-- Alerta bugs:
--
-- * can't update password without also passing provider=basic
--   as alerta checks the update message, not the user.
--
-- * can't set email_verified to false without providing another parameter.
--
-- The helper functions "withUserName" etc. can be used in conjunction with
-- the default empty "UserAttr" to build up a nonempty "UserAttr".

data UserAttr (u :: IsEmpty) = UserAttr
  { userAttrName           :: Maybe UserName
  , userAttrLogin          :: Maybe Email
  , userAttrPassword       :: Maybe Password
  , userAttrProvider       :: Maybe Provider
  , userAttrText           :: Maybe Text
  , userAttrEmail_verified :: Maybe Bool
  } deriving (Eq, Show, Generic)

emptyUserAttr :: UserAttr 'Empty
emptyUserAttr = UserAttr Nothing Nothing Nothing Nothing Nothing Nothing

checkNonempty :: UserAttr u -> Either (UserAttr 'Empty) (UserAttr 'Nonempty)
checkNonempty a@(UserAttr Nothing Nothing Nothing Nothing Nothing Nothing) = Left  $ coerce a
checkNonempty a                                                            = Right $ coerce a

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

-- TODO replace with lenses
withUserName          :: UserAttr u -> UserName -> UserAttr 'Nonempty
withUserLogin         :: UserAttr u -> Email    -> UserAttr 'Nonempty
withUserPassword      :: UserAttr u -> Password -> UserAttr 'Nonempty
withUserProvider      :: UserAttr u -> Provider -> UserAttr 'Nonempty
withUserText          :: UserAttr u -> Text   -> UserAttr 'Nonempty
withUserEmailVerified :: UserAttr u -> Bool     -> UserAttr u
withUserName          u s = u { userAttrName = Just s }
withUserLogin         u s = u { userAttrLogin = Just s }
-- sets the provider to basic to work around the bug mentioned above
withUserPassword      u s = u { userAttrPassword = Just s, userAttrProvider = Just "basic" }
withUserProvider      u s = u { userAttrProvider = Just s }
withUserText          u s = u { userAttrText = Just s }
withUserEmailVerified u b = u { userAttrEmail_verified = Just b }

data UserInfo = UserInfo
  { userInfoCreateTime     :: UTCTime
  , userInfoId             :: UUID
  , userInfoName           :: UserName
  , userInfoProvider       :: Provider
  , userInfoLogin          :: Email
  , userInfoText           :: Text
  , userInfoEmail_verified :: Bool
  } deriving (Eq, Show, Generic)

data RoleType = UserRoleType | AdminRoleType
 deriving (Eq, Ord, Bounded, Enum, Ix, Show, Generic)

data ExtendedUserInfo = ExtendedUserInfo
  { extendedUserInfoCreateTime     :: UTCTime
  , extendedUserInfoId             :: UUID
  , extendedUserInfoName           :: UserName
  , extendedUserInfoLogin          :: Email
  , extendedUserInfoProvider       :: Provider
  , extendedUserInfoRole           :: RoleType
  , extendedUserInfoText           :: Text
  , extendedUserInfoEmail_verified :: Bool
  } deriving (Eq, Show, Generic)

data UserResp = OkUserResp
  { okUserRespId   :: UUID
  , okUserRespUser :: UserInfo
  } | ErrorUserResp {
    errorUserRespMessage :: Text
  } deriving (Eq, Show, Generic)

data UserResp' = UserResp'
  { userResp'Id   :: UUID
  , userResp'User :: UserInfo
  } deriving (Eq, Show, Generic)

data UsersResp = OkUsersResp
  { okUsersRespUsers   :: [ExtendedUserInfo]
  , okUsersRespTotal   :: Int
  , okUsersRespDomains :: [Text]       -- allowed email domains
  , okUsersRespGroups  :: [Text]       -- allowed Gitlab groups
  , okUsersRespOrgs    :: [Text]       -- allowed Github orgs
  , okUsersRespRoles   :: Maybe [Text] -- allowed Keycloud roles
  , okUsersRespTime    :: UTCTime
  , okUsersRespMessage :: Maybe Text
  } | ErrorUsersResp {
    errorUsersResp :: Text
  } deriving (Eq, Show, Generic)

--------------------------------------------------------------------------------
-- Customers
--------------------------------------------------------------------------------

data Customer = Customer
  { customerCustomer :: CustomerName
  , customerMatch    :: Text -- regex, apparently
  } deriving (Eq, Show, Generic)

data CustomerInfo = CustomerInfo
  { customerInfoId       :: UUID
  , customerInfoCustomer :: CustomerName
  , customerInfoMatch    :: Text -- regex, apparently
  } deriving (Eq, Show, Generic)

data CustomerResp = OkCustomerResp
  { okCustomerRespId       :: UUID
  , okCustomerRespCustomer :: CustomerInfo
  } | ErrorCustomerResp {
    errorCustomerRespMessage :: Text
  } deriving (Eq, Show, Generic)

data CustomersResp = OkCustomersResp
  { okCustomersRespCustomers :: [CustomerInfo]
  , okCustomersRespTotal     :: Int
  , okCustomersRespMessage   :: Maybe Text
  , okCustomersRespTime      :: UTCTime
  } | ErrorCustomersResp {
    errorCustomersMessage    :: Text
  } deriving (Eq, Show, Generic)

--------------------------------------------------------------------------------
-- Responses
--------------------------------------------------------------------------------

data Response a = ErrorResponse !Text | OkResponse a
  deriving (Eq, Show, Functor)

instance Applicative Response where
  pure = OkResponse
  ErrorResponse t <*> _ = ErrorResponse t
  OkResponse f <*> r    = f <$> r

instance Monad Response where
  ErrorResponse t >>= f = ErrorResponse t
  OkResponse a >>= f    = f a

instance ToJSON a => ToJSON (Response a) where
  toJSON (OkResponse t)    = addPair ("status", "ok") $ toJSON t
  toJSON (ErrorResponse t) = object ["status" .= ("error" :: Text), "message" .= t]

instance FromJSON a => FromJSON (Response a) where
  parseJSON = withObject "object" $ \o -> do
    status <- o .: "status"
    case status of
      "ok"    -> OkResponse <$> parseJSON (Object o)
      "error" -> ErrorResponse <$> o .: "message"
      other   -> fail $ "\"" ++ other ++ "\" is not a valid status"

-- NB unsafe!
addPair :: Pair -> Aeson.Value -> Aeson.Value
addPair (k, v) (Object m) = Object $ HM.insert k v m
addPair _ other           = error $ "Can't add a pair to " ++ name
  where
  name = case other of
           Object _ -> "an object"
           Array _  -> "an array"
           String _ -> "a string"
           Number _ -> "a number"
           Bool _   -> "a boolean"
           Null     -> "null"

$( deriveJSON (toOpts 2 2 def)                    ''UserResp'            )


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
