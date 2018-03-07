{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Alerta.Golden where

import           Alerta.Gen
import           Data.Aeson                  (FromJSON, ToJSON,
                                              eitherDecodeStrict)
import           Data.Aeson.Encode.Pretty    (Config (..), Indent (Spaces),
                                              NumberFormat (Generic),
                                              encodePretty', keyOrder)
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Lazy        as LBS
import           Data.Monoid                 ((<>))
import           Data.Typeable               (Typeable)
import           Hedgehog                    (Gen)
import qualified Hedgehog.Gen                as Gen
import qualified Hedgehog.Range              as Range
import           System.FilePath             ((-<.>), (<.>), (</>))
import           Test.Tasty                  (TestTree)
import           Test.Tasty.Golden.RoundTrip (goldenTest)
import           Text.Printf                 (printf)

tests :: [TestTree]
tests =
    [ goldenRoundTrip $ genResponse genUnit
    , goldenRoundTrip genUTCTime
    , goldenRoundTrip genAlert
    , goldenRoundTrip genAlertInfo
    , goldenRoundTrip genHistoryItem
    , goldenRoundTrip genExtendedHistoryItem
    , goldenRoundTrip $ genResponse genCreateAlertResp
    , goldenRoundTrip $ genResponse genAlertResp
    , goldenRoundTrip $ genResponse genAlertsResp
    , goldenRoundTrip $ genResponse genAlertCountResp
    , goldenRoundTrip genResourceInfo
    , goldenRoundTrip genTop10Info
    , goldenRoundTrip $ genResponse genTop10Resp
    , goldenRoundTrip $ genResponse genAlertHistoryResp
    , goldenRoundTrip genEnvironmentInfo
    , goldenRoundTrip $ genResponse genEnvironmentsResp
    , goldenRoundTrip genServiceInfo
    , goldenRoundTrip genStatusChange
    , goldenRoundTrip $ genResponse genServicesResp
    , goldenRoundTrip genBlackout
    , goldenRoundTrip genBlackoutInfo
    , goldenRoundTrip genBlackoutStatus
    , goldenRoundTrip genExtendedBlackoutInfo
    , goldenRoundTrip $ genResponse genBlackoutResp
    , goldenRoundTrip $ genResponse genBlackoutsResp
    , goldenRoundTrip genHeartbeat
    , goldenRoundTrip genHeartbeatInfo
    , goldenRoundTrip $ genResponse genCreateHeartbeatResp
    , goldenRoundTrip $ genResponse genHeartbeatResp
    , goldenRoundTrip $ genResponse genHeartbeatsResp
    , goldenRoundTrip genApiKey
    , goldenRoundTrip genCreateApiKey
    , goldenRoundTrip genApiKeyInfo
    , goldenRoundTrip $ genResponse genCreateApiKeyResp
    , goldenRoundTrip $ genResponse genApiKeysResp
    , goldenRoundTrip genUser
    , goldenRoundTrip genRoleType
    , goldenRoundTrip genUserInfo
    , goldenRoundTrip genExtendedUserInfo
    , goldenRoundTrip $ genResponse genUserResp
    , goldenRoundTrip $ genResponse genUsersResp
    , goldenRoundTrip genCustomer
    , goldenRoundTrip genCustomerInfo
    , goldenRoundTrip $ genResponse genCustomerResp
    , goldenRoundTrip $ genResponse genCustomersResp
    ]

goldenRoundTrip :: forall a. (Typeable a, FromJSON a, ToJSON a) => Gen a -> TestTree
goldenRoundTrip gen =
  goldenTest name getGolden getTested cmp (Just dump) create update
    where
      name  = fmap subst $ typeName gen
      subst s = if s == ' ' then '_' else s

      file  = dir </> name <.> "json"
      dir   = "test/Alerta/Golden/files/"

      getGolden = BS.readFile file

      getTested =
        return
        . either (error . ("Decode failed: " ++)) id
        . fmap (LBS.toStrict . prettyPrint)
        . eitherDecodeStrict @[a]

      cmp x y =
        let
          err =
            printf
            "Input differed from output. Input was:\n%s\nOutput was:\n%s\n"
            (show x)
            (show y)

        in return $
          if x == y
          then Nothing
          else (Just err)

      dump o = do
        let testOutput = file -<.> "test_output.json"
        BS.writeFile testOutput o
        return $ "Output written to " ++ testOutput

      create = do
        putStrLn $ "Creating a new file " ++ file
        bs <- LBS.toStrict . prettyPrint <$> createList gen
        BS.writeFile file bs
        return bs

      update = BS.writeFile file

      createList = Gen.sample . Gen.list (Range.singleton 10)

prettyPrint :: ToJSON a => a -> LBS.ByteString
prettyPrint = encodePretty' $ Config
  { confIndent          = Spaces 2
  , confCompare         = keyOrder ["status", "error", "message", "total"] <> compare
  , confNumFormat       = Generic
  , confTrailingNewline = True
  }
