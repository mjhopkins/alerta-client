{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Alerta.Hedgehog where

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
import           Test.Tasty               (TestName, TestTree)
import           Test.Tasty.Hedgehog      (testProperty)


tests :: [TestTree]
tests = uncurry testProperty <$>
  [ roundTrip genSeverity
  , roundTripKey genSeverity
  , roundTrip genStatus
  , roundTripKey genStatus
  , roundTrip genCustomer
  , roundTrip genCreateAlertResp
  , roundTrip $ genResponse genCreateAlertResp
  , roundTrip $ genResponse genAlertResp
  , roundTrip $ genResponse genAlertsResp
  , roundTrip $ genResponse genAlertCountResp
  , roundTrip genResourceInfo
  , roundTrip genTop10Info
  , roundTrip $ genResponse genTop10Resp
  , roundTrip $ genResponse genAlertHistoryResp
  , roundTrip genStatusChange
  , roundTrip $ genResponse genUnit
  , roundTrip genTrendIndication
  , roundTrip genHistoryItem
  , roundTrip genExtendedHistoryItem
  , roundTrip genEnvironmentInfo
  , roundTrip $ genResponse genEnvironmentsResp
  , roundTrip genServiceInfo
  , roundTrip $ genResponse genServicesResp
  , roundTrip genBlackout
  , roundTrip genBlackoutInfo
  , roundTrip genBlackoutStatus
  , roundTrip genExtendedBlackoutInfo
  , roundTrip $ genResponse genBlackoutResp
  , roundTrip $ genResponse genBlackoutsResp
  , roundTrip genHeartbeat
  , roundTrip genHeartbeatInfo
  , roundTrip $ genResponse genCreateHeartbeatResp
  , roundTrip $ genResponse genHeartbeatResp
  , roundTrip $ genResponse genHeartbeatsResp
  , roundTrip genApiKey
  , roundTrip genApiKeyType
  , roundTrip genCreateApiKey
  , roundTrip genApiKeyInfo
  , roundTrip $ genResponse genCreateApiKeyResp
  , roundTrip $ genResponse genApiKeysResp
  , roundTrip genUser
  , roundTrip genRoleType
  , roundTrip genUserInfo
  , roundTrip genExtendedUserInfo
  , roundTrip $ genResponse genUserResp
  , roundTrip $ genResponse genUsersResp
  , roundTrip genCustomer
  , roundTrip genCustomerInfo
  , roundTrip $ genResponse genCustomerResp
  , roundTrip $ genResponse genCustomersResp
  ]

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
