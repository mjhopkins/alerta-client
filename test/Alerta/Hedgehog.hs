{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedLists           #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeFamilies              #-}

{-# OPTIONS_GHC -fno-warn-orphans      #-}

module Alerta.Hedgehog where

import           Alerta
import           Control.Applicative      (liftA2)
import           Control.Arrow            ((&&&))
import           Data.Aeson               hiding (Value)
import           Data.Aeson.Encode.Pretty (encodePretty)
import           Data.Bifunctor           (bimap)
import           Data.Map                 (Map)
import           Data.Monoid              ((<>))
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Data.Time                (Day, DiffTime, UTCTime (..),
                                           fromGregorian, secondsToDiffTime)
import           Data.Typeable            (Typeable, tyConName, typeRep, typeRepTyCon)
import           GHC.Exts                 (IsList(..))
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
  , roundTrip genExtendedHistoryItem
  , roundTrip genEnvironmentInfo
  , roundTrip genEnvironmentsResp
  , roundTrip genServiceInfo
  , roundTrip genServicesResp
  , roundTrip genBlackout
  , roundTrip genBlackoutInfo
  , roundTrip genBlackoutStatus
  , roundTrip genExtendedBlackoutInfo
  , roundTrip genBlackoutResp
  , roundTrip genBlackoutsResp
  , roundTrip genHeartbeat
  , roundTrip genHeartbeatInfo
  , roundTrip genCreateHeartbeatResp
  , roundTrip genHeartbeatResp
  , roundTrip genHeartbeatsResp
  , roundTrip genApiKey
  , roundTrip genApiKeyType
  , roundTrip genCreateApiKey
  , roundTrip genApiKeyInfo
  , roundTrip genCreateApiKeyResp
  , roundTrip genApiKeysResp
  , roundTrip genUser
  , roundTrip genRoleType
  , roundTrip genUserInfo
  , roundTrip genExtendedUserInfo
  , roundTrip genUserResp
  , roundTrip genUsersResp
  , roundTrip genCustomer
  , roundTrip genCustomerInfo
  , roundTrip genCustomerResp
  , roundTrip genCustomersResp
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
genUserName      :: Gen UserName
genUserName      = Gen.text [3..10] Gen.alphaNum
genCustomerName  :: Gen CustomerName
genCustomerName  = Gen.text [3..10] Gen.alphaNum
genTag           :: Gen Tag
genTag           = Gen.text [3..10] Gen.alphaNum
genEmail         :: Gen Tag
genEmail         =
  (\u d tld -> u <> "@" <> d <> "." <> tld)
  <$> Gen.text [3..10] Gen.alphaNum
  <*> Gen.text [2..10] Gen.alphaNum
  <*> Gen.element ["com", "org", "net", "com.au"]
genPassword      :: Gen Password
genPassword      = genText
genProvider      :: Gen Provider
genProvider      = genText
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
genDiffTime = secondsToDiffTime <$> Gen.integral [0..86400] -- sic: inclusive because leap seconds

-- genAttributes :: Gen Attributes
-- Attribute keys must not contain "." or "$"
-- note that key "ip" will be overwritten
genAttributes :: Gen (Map Text Text)
genAttributes = genMap [0..5] genText genText

genList :: Range Int -> Gen a -> Gen [a]
genList = Gen.list

genMaybe :: Gen a -> Gen (Maybe a)
genMaybe = Gen.maybe

genInt :: Range Int -> Gen Int
genInt = Gen.int

genBool :: Gen Bool
genBool = Gen.bool

genMap :: Ord k => Range Int -> Gen k -> Gen v -> Gen (Map k v)
genMap r k v = Gen.map r $ liftA2 (,) k v

genTimeout :: Gen Int
genTimeout = Gen.int [0..432000]

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
  , SeverityExtendedHistoryItem
      <$> genUUID
      <*> genResource
      <*> genEvent
      <*> genEnvironment
      <*> genSeverity
      <*> genList [0..5] genService
      <*> genGroup
      <*> genValue
      <*> genText
      <*> genList [0..5] genTag
      <*> genAttributes
      <*> genOrigin
      <*> genUTCTime
      <*> genMaybe genCustomerName
  ]

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

genEnvironmentInfo :: Gen EnvironmentInfo
genEnvironmentInfo = EnvironmentInfo
    <$> genInt [0..5]
    <*> genEnvironment

genEnvironmentsResp :: Gen EnvironmentsResp
genEnvironmentsResp = genResp ErrorEnvironmentsResp $ OkEnvironmentsResp
  <$> genMaybe genText
  <*> genInt [0..5]
  <*> genList [0..5] genEnvironmentInfo

genServiceInfo :: Gen ServiceInfo
genServiceInfo = ServiceInfo
  <$> genInt [0..5]
  <*> genEnvironment
  <*> genService

genStatusChange :: Gen StatusChange
genStatusChange = StatusChange
  <$> genStatus
  <*> Gen.maybe genText

genServicesResp :: Gen ServicesResp
genServicesResp = genResp ErrorServicesResp $ OkServicesResp
  <$> genInt [0..5]
  <*> genList [0..5] genServiceInfo
  <*> genMaybe genText

genBlackout :: Gen Blackout
genBlackout = Blackout
  <$> genEnvironment
  <*> genMaybe genResource
  <*> genMaybe genService
  <*> genMaybe genEvent
  <*> genMaybe genGroup
  <*> genMaybe (genList [0..5] genTag)
  <*> genMaybe genUTCTime
  <*> genMaybe genUTCTime
  <*> genMaybe (genInt [0..10])

genBlackoutInfo :: Gen BlackoutInfo
genBlackoutInfo = BlackoutInfo
  <$> genUUID
  <*> genInt [0..5]
  <*> genEnvironment
  <*> genMaybe genResource
  <*> genMaybe (genList [0..5] genService)
  <*> genMaybe genEvent
  <*> genMaybe genGroup
  <*> genMaybe (genList [0..5] genTag)
  <*> genMaybe genCustomerName
  <*> genUTCTime
  <*> genUTCTime
  <*> genInt [0..10]

genBlackoutStatus :: Gen BlackoutStatus
genBlackoutStatus = Gen.enumBounded

genExtendedBlackoutInfo :: Gen ExtendedBlackoutInfo
genExtendedBlackoutInfo = ExtendedBlackoutInfo
  <$> genUUID
  <*> genInt [0..6]
  <*> genEnvironment
  <*> genMaybe genResource
  <*> genMaybe (genList [0..5] genService)
  <*> genMaybe genEvent
  <*> genMaybe genGroup
  <*> genMaybe (genList [0..5] genTag)
  <*> genMaybe genCustomerName
  <*> genUTCTime
  <*> genUTCTime
  <*> genInt [0..5]
  <*> genInt [0..5]
  <*> genBlackoutStatus

genBlackoutResp :: Gen BlackoutResp
genBlackoutResp = genResp ErrorBlackoutResp $ OkBlackoutResp
  <$> genUUID
  <*> genBlackoutInfo

genBlackoutsResp :: Gen BlackoutsResp
genBlackoutsResp = genResp ErrorBlackoutsResp $ OkBlackoutsResp
  <$> genInt [0..5]
  <*> genList [0..5] genExtendedBlackoutInfo
  <*> genMaybe genText
  <*> genUTCTime

genHeartbeat :: Gen Heartbeat
genHeartbeat = Heartbeat
  <$> genMaybe genOrigin
  <*> genList [0..5] genTag
  <*> genMaybe genUTCTime
  <*> genMaybe (genInt [0..5])
  <*> genMaybe genCustomerName

genHeartbeatInfo :: Gen HeartbeatInfo
genHeartbeatInfo = HeartbeatInfo
  <$> genUTCTime
  <*> genMaybe genCustomerName
  <*> genHref
  <*> genUUID
  <*> genOrigin
  <*> genUTCTime
  <*> genList [0..5] genTag
  <*> genInt [0..5]
  <*> genText

genCreateHeartbeatResp :: Gen CreateHeartbeatResp
genCreateHeartbeatResp = genResp ErrorCreateHeartbeatResp $ OkCreateHeartbeatResp
  <$> genUUID
  <*> genHeartbeatInfo

genHeartbeatResp :: Gen HeartbeatResp
genHeartbeatResp = genResp ErrorHeartbeatResp $ OkHeartbeatResp
  <$> genHeartbeatInfo
  <*> genInt [0..5]

genHeartbeatsResp :: Gen HeartbeatsResp
genHeartbeatsResp = genResp ErrorHeartbeatsResp $ OkHeartbeatsResp
  <$> genList [0..5] genHeartbeatInfo
  <*> genMaybe genUTCTime
  <*> genInt [0..5]
  <*> genMaybe genText

genApiKey :: Gen ApiKey
genApiKey = ApiKey <$> Gen.text (Range.singleton 40) Gen.unicode

genApiKeyType :: Gen ApiKeyType
genApiKeyType = Gen.enumBounded

genCreateApiKey :: Gen CreateApiKey
genCreateApiKey = CreateApiKey
  <$> genMaybe genEmail
  <*> genMaybe genCustomerName
  <*> genMaybe genApiKeyType
  <*> genMaybe genText

genApiKeyInfo :: Gen ApiKeyInfo
genApiKeyInfo = ApiKeyInfo
  <$> genEmail
  <*> genApiKey
  <*> genApiKeyType
  <*> genText
  <*> genUTCTime
  <*> genInt [0..5]
  <*> genMaybe genUTCTime
  <*> genMaybe genCustomerName

genCreateApiKeyResp :: Gen CreateApiKeyResp
genCreateApiKeyResp = genResp ErrorCreateApiKeyResp $ OkCreateApiKeyResp
  <$> genApiKey
  <*> genApiKeyInfo

genApiKeysResp :: Gen ApiKeysResp
genApiKeysResp = genResp ErrorApiKeysResp $ OkApiKeysResp
  <$> genList [0..5] genApiKeyInfo
  <*> genInt [0..5]
  <*> genUTCTime
  <*> genMaybe genText

genUser :: Gen User
genUser = User
  <$> genUserName
  <*> genEmail
  <*> genPassword
  <*> genMaybe genProvider
  <*> genMaybe genText
  <*> genMaybe genBool

genRoleType :: Gen RoleType
genRoleType = Gen.enumBounded

genUserInfo :: Gen UserInfo
genUserInfo = UserInfo
  <$> genUTCTime
  <*> genUUID
  <*> genUserName
  <*> genProvider
  <*> genEmail
  <*> genText
  <*> genBool

genExtendedUserInfo :: Gen ExtendedUserInfo
genExtendedUserInfo = ExtendedUserInfo
  <$> genUTCTime
  <*> genUUID
  <*> genUserName
  <*> genEmail
  <*> genProvider
  <*> genRoleType
  <*> genText
  <*> genBool

genUserResp :: Gen UserResp
genUserResp = genResp ErrorUserResp $ OkUserResp
  <$> genUUID
  <*> genUserInfo

genUsersResp :: Gen UsersResp
genUsersResp = genResp ErrorUsersResp $ OkUsersResp
  <$> genList [0..5] genExtendedUserInfo
  <*> genInt [0..5]
  <*> genList [0..5] genText
  <*> genList [0..5] genText
  <*> genList [0..5] genText
  <*> genMaybe (genList [0..5] genText)
  <*> genUTCTime
  <*> genMaybe genText

genCustomer :: Gen Customer
genCustomer = Customer
  <$> genCustomerName
  <*> genRegex

genCustomerInfo :: Gen CustomerInfo
genCustomerInfo = CustomerInfo
  <$> genUUID
  <*> genCustomerName
  <*> genRegex

genCustomerResp :: Gen CustomerResp
genCustomerResp = genResp ErrorCustomerResp $ OkCustomerResp
  <$> genUUID
  <*> genCustomerInfo

genCustomersResp :: Gen CustomersResp
genCustomersResp = genResp ErrorCustomersResp $ OkCustomersResp
  <$> genList [0..5] genCustomerInfo
  <*> genInt [0..5]
  <*> genMaybe genText
  <*> genUTCTime

genResp' :: Gen Resp
genResp' = genResp ErrorResp $ pure OkResp

roundTrip :: (Typeable a, Show a, Eq a, FromJSON a, ToJSON a) => Gen a -> (TestName, Property)
roundTrip = mkTest prop_trip (++ " round-trip")

roundTripKey :: (Typeable k, Show k, Ord k, ToJSONKey k, FromJSONKey k) => Gen k -> (TestName, Property)
roundTripKey = mkTest prop_tripKey (++ " key round-trip")

mkTest :: Typeable a => (p a -> PropertyT IO ()) -> (String -> String) -> p a -> (TestName, Property)
mkTest prop mkTestName = mkTestName . typeName &&& property . prop

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
