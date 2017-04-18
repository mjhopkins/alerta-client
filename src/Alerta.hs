{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Alerta where

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
  =    "alert" :> ReqBody '[JSON] Alert :> Post '[JSON] CreateAlertResp
  :<|> "alert" :> Capture "id" UUID     :> Get '[JSON] AlertResp
  :<|> "alert" :> Capture "id" UUID     :> Delete '[JSON] Resp
  :<|> "alert" :> Capture "id" UUID     :> "status"      :> ReqBody '[JSON] StatusChange :> Put '[JSON] Resp
  :<|> "alert" :> Capture "id" UUID     :> "tag"         :> ReqBody '[JSON] Tags         :> Put '[JSON] Resp
  :<|> "alert" :> Capture "id" UUID     :> "untag"       :> ReqBody '[JSON] Tags         :> Put '[JSON] Resp
  :<|> "alert" :> Capture "id" UUID     :> "attributes"  :> ReqBody '[JSON] Attributes   :> Put '[JSON] Resp

type Query a   = a --TODO
type Limited a = QueryParam "limit" Limit :> a

-- An environment cannot be created – it is a dynamically derived resource based on existing alerts.
type EnvironmentApi
 = "environments" :> Limited (Query (Get '[JSON] EnvironmentsResp))
-- 
-- A service cannot be created – it is a dynamically derived resource based on existing alerts.
type ServiceApi
 = "services" :> Limited (Query (Get '[JSON] ServicesResp))

type BlackoutApi
 =    "blackout"  :> ReqBody '[JSON] Blackout :> Post '[JSON] BlackoutResp
 :<|> "blackout"  :> Capture "blackout" UUID  :> Delete '[JSON] Resp
 :<|> "blackouts" :> Get '[JSON] BlackoutsResp

type HeartbeatApi
 =     "heartbeat"  :> ReqBody '[JSON] Heartbeat :> Post '[JSON] CreateHeartbeatResp
 :<|>  "heartbeat"  :> Capture "id" UUID         :> Get '[JSON] HeartbeatResp
 :<|>  "heartbeat"  :> Capture "id" UUID         :> Delete '[JSON] Resp
 :<|>  "heartbeats" :> Get '[JSON] HeartbeatsResp

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

