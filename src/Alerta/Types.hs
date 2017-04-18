{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}

module Alerta.Types where

import           Alerta.Util

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Aeson.TH
import           Data.Default
import           Data.Map                  (Map)
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
type UUID        = String -- TODO actual UUID type?
type Href        = String -- TODO use URL type for hrefs

data Severity =
    Security      -- 0     Black
  | Critical      -- 1     Red
  | Major         -- 2     Orange
  | Minor         -- 3     Yellow
  | Warning       -- 4     Blue
  | Indeterminate -- 5     Silver
  | Cleared       -- 5     Green
  | Normal        -- 5     Green
  | Ok            -- 5     Green
  | Informational -- 6     Green
  | Debug         -- 7     Purple
  | Trace         -- 8     Grey
  | Unknown
  deriving (Eq, Show, Read, Ord, Generic)

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
  deriving (Eq, Show, Read, Ord, Generic)

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

data TrendIndication = MoreSevere | LessSevere | NoChange
  deriving (Show, Bounded, Enum, Generic)

instance FromHttpApiData TrendIndication where
  parseQueryParam = parseBoundedTextData

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
  } deriving (Show, Generic)


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
  } deriving (Show, Generic)

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
  } deriving (Show, Generic)

data CreateAlertResp = OkCreateAlertResp {
    okCreateAlertRespId         :: UUID
  , okCreateAlertRespAlert      :: Maybe AlertInfo -- not present if rate limited or in blackout
  , okCreateAlertRespMessage    :: Maybe Text      -- present when rate limited or in blackout
  } | ErrorCreateAlertResp {
    errorCreateAlertRespMessage :: Text
  } deriving (Show, Generic)

data AlertResp = OkAlertResp {
    okAlertRespAlert      :: AlertInfo
  , okAlertRespTotal      :: Int
  } | ErrorAlertResp {
    errorAlertRespMessage :: Text
  } deriving (Show, Generic)

newtype Tags = Tags { tags :: [Tag] } deriving (Show, Generic)

newtype Attributes = Attributes { attributes :: Map String String } deriving (Show, Generic)

data StatusChange = StatusChange {
    statusChangeStatus :: Status -- docs say not "unknown" but the api allows it (in fact any text is accepted)
  , statusChangeText   :: Maybe String
  } deriving (Show, Generic)

data Resp = OkResp | ErrorResp { respMessage :: String }
  deriving (Show, Generic)


$( deriveJSON (toOpts 0 0 def)                      ''Severity         )
$( deriveJSON (toOpts 1 1 def)                      ''Status           )
$( deriveJSON (toOpts 1 1 def)                      ''Alert            )
$( deriveJSON (toOpts 0 0 def { unwrap = False })   ''Tags             )
$( deriveJSON (toOpts 0 0 def { unwrap = False })   ''Attributes       )
$( deriveJSON (toOpts 3 2 def)                      ''AlertResp        )
$( deriveJSON (toOpts 4 3 def)                      ''CreateAlertResp  )
$( deriveJSON (toOpts 2 2 def)                      ''AlertInfo        )
$( deriveJSON (toOpts 2 2 def)                      ''StatusChange     )
$( deriveJSON (toOpts 2 1 def)                      ''Resp             )
$( deriveJSON (toOpts 0 0 def)                      ''TrendIndication  )
$( deriveJSON (toOpts 2 2 def { tag = "type" })     ''HistoryItem      )