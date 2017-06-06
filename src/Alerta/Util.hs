module Alerta.Util
  ( toOpts
  , AesonOpts(..)
  , showTextLowercase
  , capitalise
  , uncapitalise
  , onCamelComponents
  , dropRight
  ) where

import           Data.Aeson.TH
import           Data.Char
import           Data.Default             
import qualified Data.Text                as T
import           Data.Text                (Text)

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

