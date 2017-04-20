{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Alerta where

import           Alerta.Auth
import           Alerta.Types
import           Servant
import           Servant.Client

{-
The alerta API allows you to query/modify
  * Alerts
  * Environments
  * Services
  * Blackouts
  * Heartbeats
  * Api Keys
  * Users
  * Customers
-}

type AlertApi
  =    WithApiKey :> "alert" :> ReqBody '[JSON] Alert :> Post '[JSON] CreateAlertResp
  :<|> WithApiKey :> "alert" :> Capture "id" UUID     :> Get '[JSON] AlertResp
  :<|> WithApiKey :> "alert" :> Capture "id" UUID     :> Delete '[JSON] Resp
  :<|> WithApiKey :> "alert" :> Capture "id" UUID     :> "status"      :> ReqBody '[JSON] StatusChange :> Put '[JSON] Resp
  :<|> WithApiKey :> "alert" :> Capture "id" UUID     :> "tag"         :> ReqBody '[JSON] Tags         :> Put '[JSON] Resp
  :<|> WithApiKey :> "alert" :> Capture "id" UUID     :> "untag"       :> ReqBody '[JSON] Tags         :> Put '[JSON] Resp
  :<|> WithApiKey :> "alert" :> Capture "id" UUID     :> "attributes"  :> ReqBody '[JSON] Attributes   :> Put '[JSON] Resp

type Query a   = a --TODO
type Limited a = QueryParam "limit" Limit :> a

-- An environment cannot be created – it is a dynamically derived resource based on existing alerts.
type EnvironmentApi
 = WithApiKey :> "environments" :> Limited (Query (Get '[JSON] EnvironmentsResp))
-- 
-- A service cannot be created – it is a dynamically derived resource based on existing alerts.
type ServiceApi
 = WithApiKey :> "services" :> Limited (Query (Get '[JSON] ServicesResp))

type BlackoutApi
 =    WithApiKey :> "blackout"  :> ReqBody '[JSON] Blackout :> Post '[JSON] BlackoutResp
 :<|> WithApiKey :> "blackout"  :> Capture "blackout" UUID  :> Delete '[JSON] Resp
 :<|> WithApiKey :> "blackouts" :> Get '[JSON] BlackoutsResp

type HeartbeatApi
 =     WithApiKey :> "heartbeat"  :> ReqBody '[JSON] Heartbeat :> Post '[JSON] CreateHeartbeatResp
 :<|>  WithApiKey :> "heartbeat"  :> Capture "id" UUID         :> Get '[JSON] HeartbeatResp
 :<|>  WithApiKey :> "heartbeat"  :> Capture "id" UUID         :> Delete '[JSON] Resp
 :<|>  WithApiKey :> "heartbeats" :> Get '[JSON] HeartbeatsResp

type ApiKeyApi
 =     NeedApiKey :> "key"  :> ReqBody '[JSON] CreateApiKey :> Post '[JSON] CreateApiKeyResp
 :<|>  WithApiKey :> "key"  :> Capture "key" ApiKey         :> Delete '[JSON] Resp
 :<|>  WithApiKey :> "keys" :> Get '[JSON] ApiKeysResp

type UserApi
 =    WithApiKey :> "user"  :> ReqBody '[JSON] User     :> Post '[JSON] UserResp
 :<|> WithApiKey :> "user"  :> Capture "user" UUID      :> Delete '[JSON] Resp
 :<|> WithApiKey :> "user"  :> Capture "user" UUID      :> ReqBody '[JSON] (UserAttr 'Nonempty) :> Put '[JSON] Resp
 :<|> WithApiKey :> "users" :> QueryParam "name" String :> QueryParam "login" Email             :> Get '[JSON] UsersResp

type CustomerApi
 =     WithApiKey :> "customer"  :> ReqBody '[JSON] Customer :> Post '[JSON] CustomerResp
 :<|>  WithApiKey :> "customer"  :> Capture "customer" UUID  :> Delete '[JSON] Resp
 :<|>  WithApiKey :> "customers" :> Get '[JSON] CustomersResp

createAlert            :<|> 
 getAlert              :<|> 
 deleteAlert           :<|> 
 setAlertStatus        :<|> 
 tagAlert              :<|> 
 untagAlert            :<|> 
 updateAlertAttributes = client (Proxy :: Proxy AlertApi)

listEnvironments = client (Proxy :: Proxy EnvironmentApi)

listServices = client (Proxy :: Proxy ServiceApi)

createBlackout  :<|>
 deleteBlackout :<|>
 listBlackouts = client (Proxy :: Proxy BlackoutApi)

createHeartbeat  :<|>
 getHeartbeat    :<|>
 deleteHeartbeat :<|>
 listHeartbeats = client (Proxy :: Proxy HeartbeatApi)

createApiKey   :<|>
  deleteApiKey :<|>
  listApiKeys = client (Proxy :: Proxy ApiKeyApi)

createUser  :<|>
 deleteUser :<|>
 updateUser :<|>
 listUsers = client (Proxy :: Proxy UserApi)

createCustomer  :<|>
 deleteCustomer :<|>
 listCustomers = client (Proxy :: Proxy CustomerApi)
