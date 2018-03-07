--------------------------------------------------------------------------------
-- |
-- Module: Alerta.Util
--
-- Internal utilities used by the Alerta package.
-- e.g. Aeson helper functions.
--------------------------------------------------------------------------------
module Alerta.Util
  ( toOpts
  , AesonOpts(..)
  , showTextLowercase
  , capitalise
  , uncapitalise
  , onCamelComponents
  , dropRight
  ) where

import           Data.Aeson.TH            as Aeson
import           Data.Char
import           Data.Default             
import qualified Data.Text                as T
import           Data.Text                (Text)

data AesonOpts = AesonOpts { tag :: String, unwrap :: Bool }

instance Default AesonOpts where
  def = AesonOpts "status" True

-- TODO increment num words dropped from fieldLabel when tag is set, otherwise don't use TaggedObject
toOpts :: Int -> Int -> AesonOpts -> Aeson.Options
toOpts k n (AesonOpts f u) = Aeson.defaultOptions {
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
dropRight i as = go as (drop i as)
  where
    go (b:bs) (_:cs) = b : go bs cs
    go _ _           = []

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

