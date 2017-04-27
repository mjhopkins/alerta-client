{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}

module Alerta.ServantExtras where

import           Alerta.Types

import           Data.List
import           Data.Monoid        ((<>))
import           Data.Proxy
import qualified Data.Text          as T

import           Servant.Common.Req (Req, appendToQueryString)
import           Servant.Client
import           Servant.API        ((:>))
import           Web.HttpApiData

-- | We need this because Alerta for some reason requires `field` and `field!`
-- parameters to be joined together with a comma rather than passed in the
-- usual way.
instance {-# OVERLAPPABLE #-} ToHttpApiData a => ToHttpApiData [a] where
  toQueryParam  = T.intercalate "," . map toQueryParam

data FieldQueries

instance HasClient api => HasClient (FieldQueries :> api) where
  type Client (FieldQueries :> api) = [FieldQuery] -> Client api

  clientWithRoute Proxy req fqs =
    clientWithRoute (Proxy :: Proxy api) $ foldl' (flip f) req fqs where

      f :: FieldQuery -> Req -> Req
      f (a,t,s,b) = appendToQueryString k (Just v) where
        k = toUrlPiece a <> suffix
        v = prefix <> T.pack s
        suffix = if b then "" else "!"
        prefix = case t of
          Regex   -> "~"
          Literal -> ""
