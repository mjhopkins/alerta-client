{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Alerta.Response
  ( Response(..)
  , foldResponse
  , bimapResponse
  , mapErrorText
  ) where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson.Types
import qualified Data.HashMap.Strict as HM
import           Data.Text           (Text)
import qualified Data.Vector         as V

--------------------------------------------------------------------------------
-- Response monad
--------------------------------------------------------------------------------

data Response a = ErrorResponse !Text | OkResponse !a
  deriving (Eq, Show)

foldResponse :: (Text -> b) -> (a -> b) -> Response a -> b
foldResponse err _ (ErrorResponse t) = err t
foldResponse _ ok  (OkResponse a)    = ok a

bimapResponse :: (Text -> Text) -> (a -> b) -> Response a -> Response b
bimapResponse err ok = foldResponse (ErrorResponse . err) (OkResponse . ok)

mapErrorText :: (Text -> Text) -> Response a -> Response a
mapErrorText = flip bimapResponse id

instance Functor Response where
  fmap = bimapResponse id

instance Applicative Response where
  pure = OkResponse
  ErrorResponse t <*> _ = ErrorResponse t
  OkResponse f <*> r    = f <$> r

instance Alternative Response where
  empty = mzero
  (<|>) = mplus

instance Monad Response where
  ErrorResponse t >>= _ = ErrorResponse t
  OkResponse a >>= f    = f a

instance MonadPlus Response where
  mzero = ErrorResponse ""
  ErrorResponse _ `mplus` r = r
  OkResponse a `mplus` _    = OkResponse a

instance ToJSON a => ToJSON (Response a) where
  toJSON (OkResponse t)    = addPair ("status", "ok") $ toJSON t
  toJSON (ErrorResponse t) = object ["status" .= ("error" :: Text), "message" .= t]

instance {-# OVERLAPPING #-} FromJSON (Response ()) where
  parseJSON = parseResponse $ \_ -> return ()

instance {-# OVERLAPPABLE #-} FromJSON a => FromJSON (Response a) where
  parseJSON = parseResponse parseJSON

parseResponse :: (Value -> Parser a) -> Value -> Parser (Response a)
parseResponse v = withObject "object" $ \o -> do
  status <- o .: "status"
  case status of
    "ok"    -> OkResponse <$> v (Object o)
    "error" -> ErrorResponse <$> o .: "message"
    other   -> fail $ "\"" ++ other ++ "\" is not a valid status"

-- NB unsafe!
addPair :: Pair -> Value -> Value
addPair (k, v) (Object m) = Object $ HM.insert k v m
addPair (k, v) (Array a) | V.null a = Object $ HM.singleton k v
addPair _ other           = error $ "Can't add a pair to " ++ name
  where
  name = case other of
           Object _ -> "an object"
           Array _  -> "an array"
           String _ -> "a string"
           Number _ -> "a number"
           Bool _   -> "a boolean"
           Null     -> "null"
