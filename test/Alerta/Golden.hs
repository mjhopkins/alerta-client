{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Alerta.Golden where

import           Alerta.Gen
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.Trans.Maybe   (runMaybeT)
import           Data.Aeson                  (FromJSON, ToJSON,
                                              eitherDecodeStrict, encode)
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Lazy        as LBS
import           Data.Functor.Identity       (runIdentity)
import           Data.Maybe                  (fromMaybe)
import           Data.Typeable               (Typeable)
import           Hedgehog                    (Gen)
import           Hedgehog.Internal.Gen       (runGenT)
import qualified Hedgehog.Internal.Gen       as Gen
import           Hedgehog.Internal.Seed      as Seed
import           Hedgehog.Internal.Tree      as Tree
import           Hedgehog.Range              as Range
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
        bs <- LBS.toStrict . encode <$> createList
        BS.writeFile file bs
        return bs

      update = BS.writeFile file

      createList :: IO [a]
      createList =
        fmap (fromMaybe err) . get . Gen.list (Range.singleton n) $ gen
        where
          n = 10
          err = error "Generator returned no results"

get :: MonadIO m => Gen a -> m (Maybe a)
get gen = do
  seed <- liftIO Seed.random
  let num = 10
  let tree = runIdentity . runMaybeT . runTree . runGenT num seed $ gen
  return $ nodeValue <$> tree
