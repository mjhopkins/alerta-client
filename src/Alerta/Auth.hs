{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

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

import           Alerta.Types       (ApiKey (..))

import           Data.Monoid        ((<>))
import           Data.Proxy
import           Data.Typeable      (Typeable)
import           Servant.API        ((:>))
import           Servant.Client
import           Servant.Common.Req (Req, addHeader)

data NeedApiKey deriving Typeable
data WithApiKey deriving Typeable

-- | Authenticate a request using an API key
addApiKey :: ApiKey -> Req -> Req
addApiKey (ApiKey key) = addHeader "Authorization" ("Key " <> key)

type instance AuthClientData NeedApiKey = ApiKey

instance HasClient api => HasClient (NeedApiKey :> api) where
  type Client (NeedApiKey :> api) = ApiKey -> Client api

  clientWithRoute _ req k =
    clientWithRoute (Proxy :: Proxy api) (addApiKey k req)

type instance AuthClientData WithApiKey = Maybe ApiKey

instance HasClient api => HasClient (WithApiKey :> api) where
  type Client (WithApiKey :> api) = Maybe ApiKey -> Client api

  clientWithRoute _ req m =
    clientWithRoute (Proxy :: Proxy api) (maybe id addApiKey m req)
