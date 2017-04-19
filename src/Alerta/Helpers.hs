{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Alerta.Helpers where

import           Control.Monad            (void)

import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import           Data.Bifunctor           (bimap)
import qualified Data.Text                as T
import           Data.Text                (Text)
import           Data.Time                (getCurrentTime, UTCTime)

import           Network.HTTP.Client      (Manager, defaultManagerSettings, newManager)

import           Servant.Client
import           System.IO.Unsafe

--------------------------------------------------------------------------------
-- utilities for manual testing and interaction with api
--------------------------------------------------------------------------------


clientEnv :: Manager -> ClientEnv
clientEnv m = ClientEnv m (BaseUrl Http "localhost" 8080 "")

run :: (Show a, FromJSON a, ToJSON a) => ClientM a -> IO a
run cl = do
  manager <- newManager defaultManagerSettings
  res <- runClientM cl (clientEnv manager)
  either handleError (\r -> putStrLn "Success" >> prettyPrint r >> return r) res

run' :: (Show a, FromJSON a, ToJSON a) => ClientM a -> IO ()
run' = void . run

handleError :: (FromJSON a, ToJSON a) => ServantError -> IO a
handleError (FailureResponse status ct b)  = either fail (\r -> putStrLn ("Failure response (status " ++ show status ++ ")") >> prettyPrint r >> return r) (eitherDecode b)
handleError (DecodeFailure e ct b)         = fail ("decode failure\n\n" ++ e ++ "\n\n" ++ showUnescaped b)
handleError (UnsupportedContentType ct b)  = fail ("unsupported content type\n\n" ++ show ct ++ "\n" ++ show b)
handleError (InvalidContentTypeHeader h b) = fail ("unsupported content type type\n\n" ++ show h ++ "\n" ++ show b)
handleError (ConnectionError e)            = fail ("connection error\n\n" ++ show e)

showUnescaped :: Show a => a -> String
showUnescaped = replace [("\\n", "\n"), ("\\\"", "\"")] . show

prettyPrint :: ToJSON a => a -> IO ()
prettyPrint = putStrLn . showUnescaped . encodePretty

replace :: [(String, String)] -> String -> String
replace assocs s = T.unpack $ foldr replace' (T.pack s) assocs'
  where
        assocs' :: [(Text, Text)]
        assocs'   = map (bimap T.pack T.pack) assocs
        replace' :: (Text, Text) -> Text -> Text
        replace' (a,b) = T.replace a b

printEncoding :: ToJSON a => a -> IO ()
printEncoding = putStrLn . showUnescaped . encodePretty

now :: () -> UTCTime
now _ = unsafePerformIO getCurrentTime