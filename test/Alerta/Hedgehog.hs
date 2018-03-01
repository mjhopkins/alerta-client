{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Alerta.Hedgehog where

import           Alerta
import           Alerta.Gen
import           Control.Arrow            ((&&&))
import           Data.Aeson               (FromJSON, FromJSONKey, ToJSON,
                                           ToJSONKey, decode, eitherDecode,
                                           encode)
import           Data.Aeson.Encode.Pretty (encodePretty)
import           Data.Bifunctor           (bimap)
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Data.Typeable            (Typeable)
import           Hedgehog                 (Gen, Property, PropertyT, forAll,
                                           property, tripping)
import qualified Hedgehog.Gen             as Gen

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
  , compat genUserResp genRespUserResp responseUserRespToUserResp
  , compat genRespUserResp genUserResp userRespToResponseUserResp
  ]

userRespToResponseUserResp :: UserResp -> Response UserResp'
userRespToResponseUserResp (ErrorUserResp t) = (ErrorResponse t)
userRespToResponseUserResp (OkUserResp i u)  = (OkResponse (UserResp' i u))

responseUserRespToUserResp :: Response UserResp' -> UserResp
responseUserRespToUserResp (ErrorResponse t)            = ErrorUserResp t
responseUserRespToUserResp (OkResponse (UserResp' i u)) = OkUserResp i u

roundTrip ::
  ( Typeable a
  , ToJSON a
  , FromJSON a
  , Eq a
  , Show a
  ) => Gen a -> (TestName, Property)
roundTrip = mkTest (++ " round-trip") prop_trip

roundTripKey ::
  ( Typeable k
  , ToJSONKey k
  , FromJSONKey k
  , Ord k
  , Show k
  ) => Gen k -> (TestName, Property)
roundTripKey = mkTest (++ " key round-trip") prop_tripKey

compat ::
  ( Typeable a
  , Typeable b
  , ToJSON a
  , FromJSON b
  , Eq a
  , Show a
  ) => Gen a -> Gen b -> (b -> a) -> (TestName, Property)
compat gena genb f =
  ( "Compatibility between "  ++ typeName gena ++ " and " ++ typeName genb
  , property (prop_compat gena genb f)
  )

mkTest :: Typeable a => (String -> String) -> (p a -> PropertyT IO ()) -> p a -> (TestName, Property)
mkTest mkTestName prop = mkTestName . typeName &&& property . prop

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

prop_compat ::
  ( ToJSON a
  , FromJSON b
  , Eq a
  , Show a
  , Monad m
  ) => Gen a -> Gen b -> (b -> a) -> PropertyT m ()
prop_compat gen1 gen2 f = do
  a <- forAll gen1
  tripping a encode (fmap f . decode)

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
