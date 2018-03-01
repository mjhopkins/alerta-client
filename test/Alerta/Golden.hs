{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Alerta.Golden where

import           Alerta.Gen
import           Data.Aeson                  (FromJSON, ToJSON,
                                              eitherDecodeStrict, encode)
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Lazy        as LBS
import           Data.Typeable               (Typeable)
import           Hedgehog                    (Gen)
import qualified Hedgehog.Gen                as Gen
import qualified Hedgehog.Range              as Range
import           Test.Tasty                  (TestTree, testGroup)
import           Test.Tasty.Golden.RoundTrip (goldenTest)
import           Text.Printf                 (printf)

tests :: IO TestTree
tests =
  return $ testGroup "Golden tests"
    [ goldenRoundTrip genUTCTime
    , goldenRoundTrip genAlert
    , goldenRoundTrip genAlertInfo
    , goldenRoundTrip genHistoryItem
    , goldenRoundTrip genExtendedHistoryItem
    , goldenRoundTrip genResp'
    , goldenRoundTrip genCreateAlertResp
    , goldenRoundTrip genAlertResp
    , goldenRoundTrip genAlertsResp
    , goldenRoundTrip genAlertCountResp
    , goldenRoundTrip genResourceInfo
    , goldenRoundTrip genTop10Info
    , goldenRoundTrip genTop10Resp
    , goldenRoundTrip genAlertHistoryResp
    , goldenRoundTrip genEnvironmentInfo
    , goldenRoundTrip genEnvironmentsResp
    , goldenRoundTrip genServiceInfo
    , goldenRoundTrip genStatusChange
    , goldenRoundTrip genServicesResp
    , goldenRoundTrip genBlackout
    , goldenRoundTrip genBlackoutInfo
    , goldenRoundTrip genBlackoutStatus
    , goldenRoundTrip genExtendedBlackoutInfo
    , goldenRoundTrip genBlackoutResp
    , goldenRoundTrip genHeartbeat
    , goldenRoundTrip genHeartbeatResp
    , goldenRoundTrip genHeartbeatsResp
    , goldenRoundTrip genApiKey
    , goldenRoundTrip genCreateApiKey
    , goldenRoundTrip genApiKeyInfo
    , goldenRoundTrip genCreateApiKeyResp
    , goldenRoundTrip genApiKeysResp
    , goldenRoundTrip genUser
    , goldenRoundTrip genRoleType
    , goldenRoundTrip genUserInfo
    , goldenRoundTrip genExtendedUserInfo
    , goldenRoundTrip genExtendedUserInfo
    , goldenRoundTrip genUserResp
    , goldenRoundTrip genUsersResp
    , goldenRoundTrip genCustomer
    , goldenRoundTrip genCustomerInfo
    , goldenRoundTrip genCustomersResp
    ]

goldenRoundTrip :: forall a. (Typeable a, FromJSON a, ToJSON a) => Gen a -> TestTree
goldenRoundTrip gen =
  goldenTest name getGolden getTested cmp create update
    where
      name = typeName gen
      dir = "test/Alerta/Golden/files/"
      file = dir ++ name ++ ".json"

      getGolden :: IO BS.ByteString
      getGolden = BS.readFile file

      getTested :: BS.ByteString -> IO BS.ByteString
      getTested =
        return
        . either (error . ("Decode failed: " ++)) id
        . fmap (LBS.toStrict . encode)
        . eitherDecodeStrict @[a]

      cmp x y =
        let
          err =
            printf
            "Input differed from output. Input was:\n%s\nOutput was:\n%s\n"
            (show x)
            (show y)
        in return $ if x == y then Nothing else Just err

      create = do
        putStrLn $ "Creating a new file " ++ file
        bs <- LBS.toStrict . encode <$> createList gen
        BS.writeFile file bs
        return bs

      update = BS.writeFile file

      createList :: Gen a -> IO [a]
      createList = Gen.sample . Gen.list (Range.singleton num)
        where num = 10
