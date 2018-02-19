{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedLists           #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeFamilies              #-}

{-# OPTIONS_GHC -fno-warn-orphans      #-}

module Alerta.Hedgehog where

import           Alerta
import           Control.Applicative      (liftA2, (<|>))
import           Control.Arrow
import           Data.Aeson               hiding (Value)
import           Data.Aeson.Encode.Pretty
import           Data.Bifunctor           (bimap)
import           Data.Map                 (Map)
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Data.Time                (Day, DiffTime, UTCTime (..),
                                           fromGregorian, secondsToDiffTime)
import           Data.Typeable
import           Debug.Trace
import           GHC.Exts
import           Hedgehog                 hiding (Group)
import qualified Hedgehog.Gen             as Gen
import qualified Hedgehog.Range           as Range

type TestName = String

tests :: [(TestName, Property)]
tests =
  [ roundTrip genSeverity
  , roundTripKey genSeverity
  , roundTrip genStatus
  , roundTripKey genStatus
  , roundTrip genCustomer
  , roundTrip genCreateAlertResp
  , roundTrip genAlertResp
  , roundTrip genAlertsResp
  , roundTrip genAlertCountResp
  , roundTrip genResourceInfo
  , roundTrip genTop10Info
  , roundTrip genTop10Resp
  , roundTrip genAlertHistoryResp
  , roundTrip genStatusChange
  , roundTrip genResp' --TODO
  , roundTrip genTrendIndication
  , roundTrip genHistoryItem
  -- , roundTrip genExtendedHistoryItem
  -- , roundTrip genEnvironmentInfo
  -- , roundTrip genEnvironmentsResp
  -- , roundTrip genServiceInfo
  -- , roundTrip genServicesResp
  -- , roundTrip genBlackout
  -- , roundTrip genBlackoutInfo
  -- , roundTrip genBlackoutStatus
  -- , roundTrip genExtendedBlackoutInfo
  -- , roundTrip genBlackoutResp
  -- , roundTrip genBlackoutsResp
  -- , roundTrip genHeartbeat
  -- , roundTrip genHeartbeatInfo
  -- , roundTrip genCreateHeartbeatResp
  -- , roundTrip genHeartbeatResp
  -- , roundTrip genHeartbeatsResp
  -- , roundTrip genCreateApiKey
  -- , roundTrip genApiKeyInfo
  -- , roundTrip genCreateApiKeyResp
  -- , roundTrip genApiKeysResp
  -- , roundTrip genRoleType
  -- , roundTrip genUser
  -- , roundTrip genUserInfo
  -- , roundTrip genExtendedUserInfo
  -- , roundTrip genUserResp
  -- , roundTrip genUsersResp
  -- , roundTrip genCustomer
  -- , roundTrip genCustomerInfo
  -- , roundTrip genCustomerResp
  -- , roundTrip genCustomersResp
  ]

genSeverity :: Gen Severity
genSeverity = Gen.enumBounded

genStatus :: Gen Status
genStatus = Gen.enumBounded

genTrendIndication :: Gen TrendIndication
genTrendIndication = Gen.enumBounded


-- rtK :: forall a m. (ToJSONKey a, FromJSONKey a, Eq a, Monad m) => a -> m _
-- rtK :: forall (f :: * -> *) (m :: * -> *) a.
--   ( Applicative f
--   , Eq (f a)
--   , Show (f a)
--   , Show a
--   , ToJSONKey a
--   , FromJSONKey (f a), Monad m
--   ) => Gen a -> PropertyT m ()
-- rtK gen = do
--   a <- forAll gen
--   -- t <- return $ case toJSONKey of
--   --   ToJSONKeyText f _  -> Left $ f
--   --   ToJSONKeyValue f _ -> Right $ f
--   case fromJSONKey :: FromJSONKeyFunction a of
--     -- FromJSONKeyCoerce !(aeson-1.1.2.0:Data.Aeson.Types.FromJSON.CoerceText a)
--         FromJSONKeyText f ->
--           case toJSONKey :: ToJSONKeyFunction a of
--             ToJSONKeyText t _  -> tripping a t f
--             -- ToJSONKeyValue t _ -> Right $ f

    -- FromJSONKeyTextParser !(Text -> aeson-1.1.2.0:Data.Aeson.Types.Internal.Parser a)
    -- FromJSONKeyValue !(Aeson.Value -> aeson-1.1.2.0:Data.Aeson.Types.Internal.Parser a)

  -- return $ _
{-
  FromJSONKeyCoerce !(aeson-1.1.2.0:Data.Aeson.Types.FromJSON.CoerceText a)
  | FromJSONKeyText !(Text -> a)
  | FromJSONKeyTextParser !(Text -> aeson-1.1.2.0:Data.Aeson.Types.Internal.Parser a)
  | FromJSONKeyValue !(Aeson.Value -> aeson-1.1.2.0:Data.Aeson.Types.Internal.Parser a)
-}

-- Email
-- Password
-- Provider
-- ShouldReverse
-- Limit
-- PageNo
-- UUID
genResource      :: Gen Resource
genResource      = Gen.text [3..10] Gen.alphaNum
genEvent         :: Gen Event
genEvent         = Gen.text [3..10] Gen.alphaNum
genService       :: Gen Service
genService       = Gen.text [3..10] Gen.alphaNum
genEnvironment   :: Gen Environment
genEnvironment   = Gen.text [3..10] Gen.alphaNum
genGroup         :: Gen Group
genGroup         = Gen.text [3..10] Gen.alphaNum
genValue         :: Gen Value
genValue         = Gen.text [3..10] Gen.alphaNum
genText          :: Gen Text
genText          = Gen.text [3..10] Gen.alphaNum
genOrigin        :: Gen Origin
genOrigin        = Gen.text [3..10] Gen.alphaNum
genAlertType     :: Gen AlertType
genAlertType     = Gen.text [3..10] Gen.alphaNum
-- genUserName      = Gen.text [3..10] Gen.alphaNum
genCustomerName  :: Gen CustomerName
genCustomerName  = Gen.text [3..10] Gen.alphaNum
genTag           :: Gen Tag
genTag           = Gen.text [3..10] Gen.alphaNum
genRegex         :: Gen Text --TODO Regex
genRegex         = Gen.text [3..10] Gen.alphaNum --TODO
genHref          :: Gen Href
genHref          = Gen.text [3..10] Gen.alphaNum --TODO
genPageNo        :: Gen PageNo
genPageNo        = Gen.int [0..100]

genUUID :: Gen UUID
genUUID = T.intercalate "-" <$> traverse (\d -> Gen.text (Range.singleton d) (Gen.enum '0' 'f')) [8, 4, 4, 4, 12]

genUTCTime :: Gen UTCTime
genUTCTime = UTCTime <$> genDay <*> genDiffTime

genDay :: Gen Day
genDay = Gen.enum (fromGregorian 1970 1 1) (fromGregorian 2060 12 31)

genDiffTime :: Gen DiffTime
genDiffTime = secondsToDiffTime <$> Gen.integral [0..864] -- sic: inclusive because leap seconds
-- genDiffTime = secondsToDiffTime <$> Gen.integral [0..86400] -- sic: inclusive because leap seconds

-- genAttributes :: Gen Attributes
-- Attribute keys must not contain "." or "$"
-- note that key "ip" will be overwritten
genAttributes :: Gen (Map Text Text)
genAttributes = genMap [0..5] genText genText

genMap :: Ord k => Range Int -> Gen k -> Gen v -> Gen (Map k v)
genMap r k v = Gen.map r $ liftA2 (,) k v

genTimeout :: Gen Int
genTimeout = Gen.int [0..432000]

genCustomer :: Gen Customer
genCustomer = Customer <$> genCustomerName <*> genRegex

genAlert :: Gen Alert
genAlert = Alert
  <$> genResource
  <*> genEvent
  <*> Gen.maybe genEnvironment
  <*> Gen.maybe genSeverity
  <*> Gen.maybe (Gen.list [0..20] genEvent)
  <*> Gen.maybe genStatus
  <*> Gen.maybe (Gen.list [0..20] genService)
  <*> Gen.maybe genGroup
  <*> Gen.maybe genValue
  <*> Gen.maybe genText
  <*> Gen.maybe (Gen.list [0..20] genTag)
  <*> Gen.maybe genAttributes
  <*> Gen.maybe genOrigin
  <*> Gen.maybe genAlertType
  <*> Gen.maybe genUTCTime
  <*> Gen.maybe genTimeout
  <*> Gen.maybe genText
  <*> Gen.maybe genCustomerName

genAlertInfo :: Gen AlertInfo
genAlertInfo = AlertInfo
  <$> genUUID
  <*> genResource
  <*> genEvent
  <*> genEnvironment
  <*> Gen.maybe genSeverity
  <*> Gen.list [0..20] genEvent
  <*> Gen.maybe genStatus
  <*> Gen.list [0..20] genService
  <*> genGroup
  <*> genValue
  <*> genText
  <*> Gen.list [0..20] genTag
  <*> genAttributes
  <*> genOrigin
  <*> genAlertType
  <*> genUTCTime
  <*> genTimeout
  <*> Gen.maybe genText
  <*> Gen.maybe genCustomerName
  <*> Gen.maybe (Gen.int [0..5])
  <*> Gen.maybe Gen.bool
  <*> Gen.maybe genSeverity
  <*> Gen.maybe genTrendIndication
  <*> genUTCTime
  <*> Gen.maybe genUUID
  <*> genUTCTime
  <*> Gen.list [0..20] genHistoryItem
  <*> genHref

genHistoryItem :: Gen HistoryItem
genHistoryItem = Gen.choice
  [ StatusHistoryItem
    <$> genEvent
    <*> genStatus
    <*> genText
    <*> genUUID
    <*> genUTCTime
  , SeverityHistoryItem
    <$> genEvent
    <*> genSeverity
    <*> genText
    <*> genUUID
    <*> genUTCTime
    <*> genValue
  ]

genExtendedHistoryItem :: Gen ExtendedHistoryItem
genExtendedHistoryItem = Gen.choice
  [ StatusExtendedHistoryItem
    <$> genUUID
    <*> genResource
    <*> genEvent
    <*> genEnvironment
    <*> genStatus
    <*> Gen.list [0..3] genService
    <*> genGroup
    <*> genText
    <*> Gen.list [0..3] genTag
    <*> genAttributes
    <*> genOrigin
    <*> genUTCTime
    <*> Gen.maybe genCustomerName
  -- , SeverityExtendedHistoryItem
  --   <$> genEvent
  --   <*> genSeverity
  --   <*> genText
  --   <*> genUUID
  --   <*> genUTCTime
  --   <*> genValue
  ]

genApiKey :: Gen ApiKey
genApiKey = ApiKey <$> Gen.text (Range.singleton 40) Gen.unicode

genResp :: (Text -> a) -> Gen a -> Gen a
genResp err ok = Gen.choice [ok, err <$> genText]

genCreateAlertResp :: Gen CreateAlertResp
genCreateAlertResp = genResp ErrorCreateAlertResp $ OkCreateAlertResp
  <$> genUUID
  <*> Gen.maybe genAlertInfo
  <*> Gen.maybe genText

genAlertResp :: Gen AlertResp
genAlertResp = genResp ErrorAlertResp $ OkAlertResp
  <$> genAlertInfo
  <*> Gen.int (Range.linear 1 10)

genAlertsResp :: Gen AlertsResp
genAlertsResp = genResp ErrorAlertsResp $ OkAlertsResp
  <$> Gen.list [0..5] genAlertInfo
  <*> Gen.int [0..100]
  <*> genPageNo
  <*> Gen.int [0..50]
  <*> Gen.int [0..50]
  <*> Gen.bool
  <*> Gen.maybe (genMap [1..3] genSeverity (Gen.int [0..10]))
  <*> Gen.maybe (genMap [1..3] genStatus (Gen.int [0..10]))
  <*> genUTCTime
  <*> Gen.bool
  <*> Gen.maybe genText

genAlertCountResp :: Gen AlertCountResp
genAlertCountResp = genResp ErrorAlertCountResp $ OkAlertCountResp
  <$> Gen.int [0..5]
  <*> Gen.int [0..5]
  <*> Gen.int [0..5]
  <*> Gen.maybe genText

genResourceInfo :: Gen ResourceInfo
genResourceInfo = ResourceInfo
  <$> genUUID
  <*> genResource
  <*> genHref

genTop10Info :: Gen Top10Info
genTop10Info = Top10Info
  <$> Gen.int [0..3]
  <*> Gen.int [0..3]
  <*> Gen.list [0..3] genEnvironment
  <*> Gen.list [0..3] genService
  <*> Gen.list [0..3] genResourceInfo

genTop10Resp :: Gen Top10Resp
genTop10Resp = genResp ErrorTop10Resp $ OkTop10Resp
  <$> Gen.list [0..4] genTop10Info
  <*> Gen.int [0..5]
  <*> Gen.maybe genText

genAlertHistoryResp :: Gen AlertHistoryResp
genAlertHistoryResp = genResp ErrorAlertHistoryResp $ OkAlertHistoryResp
  <$> Gen.list [0..4] genExtendedHistoryItem
  <*> genUTCTime
  <*> Gen.maybe genText

genStatusChange :: Gen StatusChange
genStatusChange = StatusChange
  <$> genStatus
  <*> Gen.maybe genText

genResp' :: Gen Resp
genResp' = genResp ErrorResp $ pure OkResp

{-
Severity
Status
Alert
Tags
Attributes
AlertAttr
AlertInfo
CreateAlertResp
AlertResp
AlertsResp
ResourceInfo
Top10Info
Top10Resp
AlertCountResp
AlertHistoryResp
StatusChange
Resp
TrendIndication
HistoryItem
ExtendedHistoryItem
EnvironmentInfo
EnvironmentsResp
ServiceInfo
ServicesResp
Blackout
BlackoutInfo
BlackoutStatus
ExtendedBlackoutInfo
BlackoutResp
BlackoutsResp
Heartbeat
HeartbeatInfo
CreateHeartbeatResp
HeartbeatResp
HeartbeatsResp
CreateApiKey
ApiKeyInfo
CreateApiKeyResp
ApiKeysResp
RoleType
User
UserInfo
ExtendedUserInfo
UserResp
UsersResp
Customer
CustomerInfo
CustomerResp
CustomersResp
-}

roundTrip :: (Typeable a, Show a, Eq a, FromJSON a, ToJSON a) => Gen a -> (TestName, Property)
roundTrip gen = (typeName gen ++ " round-trip", property (prop_trip gen))

roundTripKey :: (Typeable k, Show k, Ord k, ToJSONKey k, FromJSONKey k) => Gen k -> (TestName, Property)
roundTripKey gen = (typeName gen ++ " key round-trip", property (prop_tripKey gen))

typeName :: Typeable a => p a -> String
typeName = tyConName . typeRepTyCon . typeRep

prop_trip ::
  ( ToJSON a
  , FromJSON a
  , Eq a
  , Show a
  , Monad m
  ) => Gen a -> PropertyT m ()
prop_trip gen = do
  a <- forAll gen
  -- traceM (show a)
  -- let e = encode a
  -- traceM (pretty a)
  -- decode (e) === Just a
  tripping a encode eitherDecode

prop_tripKey ::
  ( ToJSONKey a
  , FromJSONKey a
  , Eq a
  , Ord a
  , Show a
  , Monad m
  ) => Gen a -> PropertyT m ()
prop_tripKey gen = do
  let g = genMap [0, 10] gen (Gen.int [0..5])
  m <- forAll g
  tripping m encode decode

instance Integral t => IsList (Range t) where
  type Item (Range t) = t
  fromList [t] = Range.singleton t
  fromList []  = error "empty range"
  fromList l   = Range.linear (head l) (last l)

  toList = error "can't turn a range into a list"

pretty :: ToJSON a => a -> String
pretty = showUnescaped . encodePretty

showUnescaped :: Show a => a -> String
showUnescaped = replace [("\\n", "\n"), ("\\\"", "\"")] . show

replace :: [(String, String)] -> String -> String
replace assocs s = T.unpack $ foldr replace' (T.pack s) assocs'
  where
    assocs' :: [(Text, Text)]
    assocs'   = map (bimap T.pack T.pack) assocs
    replace' :: (Text, Text) -> Text -> Text
    replace' (a,b) = T.replace a b


{-
OkAlertsResp
    { okAlertsRespAlerts = []
    , okAlertsRespTotal = 0
    , okAlertsRespPage = 0
    , okAlertsRespPageSize = 0
    , okAlertsRespPages = 0
    , okAlertsRespMore = False
    , okAlertsRespSeverityCounts = Nothing
    , okAlertsRespStatusCounts = Just (fromList [ ( OpenStatus , 0 ) ])
    , okAlertsRespLastTime = 1970 (-01) (-01) 00 : 00 : 00 UTC
    , okAlertsRespAutoRefresh = False
    , okAlertsRespMessage = Nothing
    }
-}

-- newtype Time = Time UTCTime

-- instance Show Time where
--   show (T)
