{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

--------------------------------------------------------------------------------
-- Module: Alerta.ServantExtras
--
-- Additions to Servant needed to cope with the peculiarities of
-- alerta's REST API.
--------------------------------------------------------------------------------
module Alerta.ServantExtras
  ( FieldQueries
  ) where

import           Alerta.Types        (FieldQuery, MatchType (..))

import           Data.List
import           Data.Monoid         ((<>))
import           Data.Proxy
import qualified Data.Text           as T
import           Servant.API         ((:>))
#if MIN_VERSION_servant_client(0,12,0)
import           Servant.Client.Core (HasClient (..), Request,
                                      appendToQueryString)
#define REQ Request
#else
import           Servant.Client
import           Servant.Common.Req  (Req, appendToQueryString)
#define REQ Req
#endif
import           Web.HttpApiData

-- | We need this because Alerta for some reason requires `field` and `field!`
-- parameters to be joined together with a comma rather than passed in the
-- usual way.
instance {-# OVERLAPPABLE #-} ToHttpApiData a => ToHttpApiData [a] where
  toQueryParam  = T.intercalate "," . map toQueryParam

data FieldQueries

#if !MIN_VERSION_servant_client(0,12,0)
instance HasClient api => HasClient (FieldQueries :> api) where
  type Client (FieldQueries :> api) = [FieldQuery] -> Client api

  clientWithRoute Proxy req fqs =
    clientWithRoute (Proxy :: Proxy api) $ foldl' (flip applyFQ) req fqs

#else
instance HasClient m api => HasClient m (FieldQueries :> api) where
  type Client m (FieldQueries :> api) = [FieldQuery] -> Client m api

  clientWithRoute m Proxy req fqs =
    clientWithRoute m (Proxy :: Proxy api) $ foldl' (flip applyFQ) req fqs

#endif

applyFQ :: FieldQuery -> REQ -> REQ
applyFQ (attr, txt, t, b) = appendToQueryString k (Just v) where
  k = toUrlPiece attr <> suffix
  v = prefix <> txt
  suffix = if b then "" else "!"
  prefix = case t of
    Regex   -> "~"
    Literal -> ""
