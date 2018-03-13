{-# LANGUAGE CPP #-}

--------------------------------------------------------------------------------
-- |
-- Module: Alerta.Helpers
--
-- Utilities for manual testing interaction with the API.
--------------------------------------------------------------------------------
module Alerta.Helpers
  ( run
  , run'
  , prettyPrintEncoding
  , now
  ) where

import           Control.Monad            (void)

import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import           Data.Bifunctor           (bimap)
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Data.Time                (UTCTime, getCurrentTime)

import           Network.HTTP.Client      (Manager, defaultManagerSettings,
                                           newManager)

import           Servant.Client
import           System.IO.Unsafe

clientEnv :: Manager -> ClientEnv
clientEnv m = ClientEnv m (BaseUrl Http "localhost" 8080 "")

-- | Run a Servant client function, pretty-printing the JSON returned.
run :: (FromJSON a, ToJSON a) => ClientM a -> IO a
run cl = do
  manager <- newManager defaultManagerSettings
  res     <- runClientM cl (clientEnv manager)
  either handleError (\r -> prettyPrint r >> putStrLn "Success" >> return r) res

-- | Run a Servant client function, pretty-printing the JSON returned, and
-- discarding the return value.
run' :: (Show a, FromJSON a, ToJSON a) => ClientM a -> IO ()
run' = void . run

handleError :: (FromJSON a, ToJSON a) => ServantError -> IO a
-- really, these should all be servant_client. But CPP.
#if !MIN_VERSION_servant(0,11,0)
handleError (FailureResponse status _ b)   = either fail (\r -> putStrLn ("Failure response (status " ++ show status ++ ")") >> prettyPrint r >> return r) (eitherDecode b)
handleError (DecodeFailure e _ b)          = fail ("decode failure\n\n" ++ e ++ "\n\n" ++ showUnescaped b)
handleError (InvalidContentTypeHeader h b) = fail ("unsupported content type type\n\n" ++ show h ++ "\n" ++ show b)
#endif
#if MIN_VERSION_servant(0,11,0) && !MIN_VERSION_servant(0,12,0)
handleError (FailureResponse _ status _ b) = either fail (\r -> putStrLn ("Failure response (status " ++ show status ++ ")") >> prettyPrint r >> return r) (eitherDecode b)
handleError (DecodeFailure e _ b)          = fail ("decode failure\n\n" ++ e ++ "\n\n" ++ showUnescaped b)
handleError (InvalidContentTypeHeader h b) = fail ("unsupported content type type\n\n" ++ show h ++ "\n" ++ show b)
#endif
#if MIN_VERSION_servant(0,12,0)
handleError (FailureResponse r) = either fail (\r -> putStrLn ("Failure response (status " ++ show status ++ ")") >> prettyPrint r >> return r) (eitherDecode b)
  where
    status = responseStatusCode r
    b      = responseBody r
handleError (DecodeFailure e r)          = fail ("decode failure\n\n" ++ T.unpack e ++ "\n\n" ++ showUnescaped b)
  where
    b = responseBody r
handleError (InvalidContentTypeHeader r) = fail ("unsupported content type type\n\n" ++ show h ++ "\n" ++ show b)
  where
    h = responseHeaders r
    b = responseBody r
#endif
handleError (UnsupportedContentType ct b)  = fail ("unsupported content type\n\n" ++ show ct ++ "\n" ++ show b)
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

-- | Pretty-print the JSON encoding of the supplied value.
prettyPrintEncoding :: ToJSON a => a -> IO ()
prettyPrintEncoding = putStrLn . showUnescaped . encodePretty

-- | Current time. Not referentially transparent!
now :: () -> UTCTime
now _ = unsafePerformIO getCurrentTime
