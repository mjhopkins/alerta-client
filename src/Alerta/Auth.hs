{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

--------------------------------------------------------------------------------
-- |
-- Module: Alerta.Auth
--
-- Auth-related additions to Servant that we use.
--------------------------------------------------------------------------------
module Alerta.Auth
  ( WithApiKey
  , NeedApiKey
  ) where

import           Alerta.Types        (ApiKey (..))

import           Data.Monoid         ((<>))
import           Data.Proxy          (Proxy (..))
import           Data.Typeable       (Typeable)
import           Servant.API         ((:>))

-- #if MIN_VERSION_servant_client(0,12,0)
#ifdef HAS_CLIENT_CORE
import           Servant.Client.Core (AuthClientData, HasClient (..), Request,
                                      addHeader)
#define REQ Request
#else
import           Servant.Client
import           Servant.Common.Req  (Req, addHeader)
#define REQ Req
#endif

data NeedApiKey deriving Typeable
data WithApiKey deriving Typeable

-- | Authenticate a request using an API key
addApiKey :: ApiKey -> REQ -> REQ
addApiKey (ApiKey key) = addHeader "Authorization" ("Key " <> key)

type instance AuthClientData NeedApiKey = ApiKey

-- #ifdef HAS_CLIENT_CORE
#if !MIN_VERSION_servant_client(0,12,0)
instance HasClient api => HasClient (NeedApiKey :> api) where
  type Client (NeedApiKey :> api) = ApiKey -> Client api

  clientWithRoute _ req k =
    clientWithRoute (Proxy :: Proxy api) (addApiKey k req)
#else
instance HasClient m api => HasClient m (NeedApiKey :> api) where
  type Client m (NeedApiKey :> api) = ApiKey -> Client m api

  clientWithRoute m _ req k =
    clientWithRoute m (Proxy :: Proxy api) (addApiKey k req)
#endif

type instance AuthClientData WithApiKey = Maybe ApiKey

-- #ifdef HAS_CLIENT_CORE
#if !MIN_VERSION_servant_client(0,12,0)
instance HasClient api => HasClient (WithApiKey :> api) where
  type Client (WithApiKey :> api) = Maybe ApiKey -> Client api

  clientWithRoute _ req m =
    clientWithRoute (Proxy :: Proxy api) (maybe id addApiKey m req)
#else
instance HasClient m api => HasClient m (WithApiKey :> api) where
  type Client m (WithApiKey :> api) = Maybe ApiKey -> Client m api

  clientWithRoute m _ req mk =
    clientWithRoute m (Proxy :: Proxy api) (maybe id addApiKey mk req)
#endif
