{-# LANGUAGE ExistentialQuantification #-}

module Test.Tasty.Golden.RoundTrip where


import           Control.DeepSeq      (rnf)
import           Control.Exception    (SomeException, evaluate, throwIO, try)
import           Data.Monoid          ((<>))
import           Data.Proxy           (Proxy (..))
import           Data.Tagged          (Tagged, untag)
import           Options.Applicative  (help, long, switch)
import           System.IO.Error      (isDoesNotExistError)
import           Test.Tasty.Options   (IsOption (..),
                                       OptionDescription (Option), lookupOption,
                                       safeRead)
import           Test.Tasty.Providers (IsTest (..), Result, TestName, TestTree,
                                       singleTest, testFailed, testPassed)

--------------------------------------------------------------------------------
-- |
-- Module Test.Tasty.Golden.RoundTrip
--
-- Extension of golden tests to support round trip testing.
--
--------------------------------------------------------------------------------

data RoundTrip =
  forall a.
    RoundTrip
     (IO a)
     (a -> IO a)
     (a -> a -> IO (Maybe String))
     (IO a)
     (a -> IO ())

goldenTest :: TestName -> IO a -> (a -> IO a) -> (a -> a -> IO (Maybe String)) -> IO a -> (a -> IO ()) -> TestTree
goldenTest name golden test cmp create update =
  singleTest name $ RoundTrip golden test cmp create update

instance IsTest RoundTrip where
  run opts golden _ = runRoundTrip golden (lookupOption opts)
  testOptions = return [Option (Proxy :: Proxy AcceptTests)]

newtype AcceptTests = AcceptTests Bool
  deriving (Eq, Ord)

instance IsOption AcceptTests where
  defaultValue = AcceptTests False
  parseValue = fmap AcceptTests . safeRead
  optionName = return "accept"
  optionHelp = return "Accept current results of golden tests"
  optionCLParser =
    fmap AcceptTests $
    switch
      (  long (untag (optionName :: Tagged AcceptTests String))
      <> help (untag (optionHelp :: Tagged AcceptTests String))
      )

runRoundTrip :: RoundTrip -> AcceptTests -> IO Result
runRoundTrip (RoundTrip getGolden getTested cmp create update) (AcceptTests accept) = do
  mbRef <- try getGolden
  case mbRef of
    Left e | isDoesNotExistError e -> do
      _ <- create --TODO fix
      return $ testPassed "Input did not exist; created"
      --TODO continue with test
    Left e    ->  throwIO e
    Right ref -> do
      mbNew <- try (getTested ref)
      case mbNew of
        Left e -> return $ testFailed $ show (e :: SomeException)
        Right new -> do
          result <- cmp ref new
          case result of
            Just _reason | accept -> do
              update new
              return $ testPassed "Accepted the new version"
            Just reason -> do
              evaluate . rnf $ reason
              return $ testFailed reason
            Nothing ->
                  return $ testPassed ""
