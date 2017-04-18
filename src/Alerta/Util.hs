{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Alerta.Util where

import           Control.Monad            (void)

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Aeson.Encode.Pretty
import           Data.Bifunctor           (bimap)
import           Data.Char                
import           Data.Default             
import qualified Data.Text                as T
import           Data.Text                (Text)
                                          
import           Network.HTTP.Client      (Manager, defaultManagerSettings, newManager)

import           Servant.Client

data AesonOpts = AesonOpts { tag :: String, unwrap :: Bool }

instance Default AesonOpts where
  def = AesonOpts "status" True

-- TODO increment num words dropped from fieldLabel when tag is set, otherwise don't use TaggedObject
toOpts :: Int -> Int -> AesonOpts -> Data.Aeson.TH.Options
toOpts k n (AesonOpts f u) = Data.Aeson.TH.Options {
    fieldLabelModifier     = uncapitalise . onCamelComponents (drop k)
  , constructorTagModifier = uncapitalise . onCamelComponents (dropRight n)
  , omitNothingFields      = True
  , allNullaryToStringTag  = True
  , unwrapUnaryRecords     = u
  , sumEncoding            = TaggedObject { tagFieldName = f, contentsFieldName = "contents" }
}

showTextLowercase :: Show a => a -> Text
showTextLowercase = T.toLower . T.pack . show

uncapitalise :: String -> String
uncapitalise []      = []
uncapitalise (h : t) = toLower h : t

capitalise :: String -> String
capitalise []      = []
capitalise (h : t) = toUpper h : t

dropRight :: Int -> [a] -> [a]
dropRight n = reverse . drop n . reverse

onCamelComponents :: ([String] -> [String]) -> String -> String
onCamelComponents f = concat . f. camelComponents

camelComponents :: String -> [String]
camelComponents = go [] ""
  where
    go :: [String] -> String -> String -> [String]
    go ws w (x:u:l:xs) | isUpper u && isLower l = go ((x:w):ws) [l, u] xs
    go ws w (l:u:xs)   | isUpper u && isLower l = go ((l:w):ws) [u] xs
    go ws w (x:xs)                              = go ws (x:w) xs
    go ws w ""                                  = reverse (map reverse (w:ws))


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

