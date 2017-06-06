{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE TypeOperators  #-}

module Alerta
  (
  -- * Types
    Limited
  , Page
  , Sort
  , Grouped
  , Fields
  , Query
  , module Alerta.Types
  , module Alerta.Helpers
  -- * alerts
  , createAlert
  , getAlert
  , deleteAlert
  , setAlertStatus
  , tagAlert
  , untagAlert
  , updateAlertAttributes
  -- * alert history and queries
  , listAlerts
  , alertHistory
  , countAlerts
  , top10
  , flappingTop10
  -- * environments
  , listEnvironments
  -- * services
  , listServices
  -- * blackouts
  , createBlackout
  , deleteBlackout
  , listBlackouts
  -- * heartbeats
  , createHeartbeat
  , getHeartbeat
  , deleteHeartbeat
  , listHeartbeats
  -- * API keys
  , createApiKey
  , deleteApiKey
  , listApiKeys
  -- * users
  , createUser
  , deleteUser
  , updateUser
  , listUsers
  -- * customers
  , createCustomer
  , deleteCustomer
  , listCustomers
  ) where

import           Alerta.Auth
import           Alerta.Helpers
import           Alerta.ServantExtras
import           Alerta.Types
import           Servant
import           Servant.Client

--------------------------------------------------------------------------------
-- The alerta API allows you to query/modify

--   * Alerts
--   * Environments
--   * Services
--   * Blackouts
--   * Heartbeats
--   * Api Keys
--   * Users
--   * Customers

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- query parameter types
--------------------------------------------------------------------------------

type Limited a = QueryParam "limit" Limit :> a

type Page a = QueryParam "page" PageNo :> a

type Sort a =
     QueryParam "reverse" ShouldReverse
  :> QueryParams "sort-by" AlertAttr
  :> a

type Grouped a =
  QueryParam "group-by" AlertAttr
  :> a

type Fields a =
     QueryParam "fields" [AlertAttr]  -- NB not QueryParams "field" AlertAttr
  :> QueryParam "fields!" [AlertAttr] -- NB not QueryParams "field!" AlertAttr
  :> a

type Query a =
     QueryParam "q" QueryString -- TODO JSON
  :> QueryParams "id" UUID
  :> QueryFlag "repeat"
  :> FieldQueries
  :> a

--------------------------------------------------------------------------------
-- API types
--------------------------------------------------------------------------------

type AlertApi =
       WithApiKey :> "alert" :> ReqBody '[JSON] Alert :> Post '[JSON] CreateAlertResp
  :<|> WithApiKey :> "alert" :> Capture "id" UUID     :> Get '[JSON] AlertResp
  :<|> WithApiKey :> "alert" :> Capture "id" UUID     :> Delete '[JSON] Resp
  :<|> WithApiKey :> "alert" :> Capture "id" UUID     :> "status"      :> ReqBody '[JSON] StatusChange :> Put '[JSON] Resp
  :<|> WithApiKey :> "alert" :> Capture "id" UUID     :> "tag"         :> ReqBody '[JSON] Tags         :> Put '[JSON] Resp
  :<|> WithApiKey :> "alert" :> Capture "id" UUID     :> "untag"       :> ReqBody '[JSON] Tags         :> Put '[JSON] Resp
  :<|> WithApiKey :> "alert" :> Capture "id" UUID     :> "attributes"  :> ReqBody '[JSON] Attributes   :> Put '[JSON] Resp

type AlertsApi =
       WithApiKey :> "alerts" :> Query (Fields (Sort (Page (Limited (Get '[JSON] AlertsResp)))))
  :<|> WithApiKey :> "alerts" :> "history" :> Query (Limited (Get '[JSON] AlertHistoryResp))
  :<|> WithApiKey :> "alerts" :> "count"   :> Query (Get '[JSON] AlertCountResp)
  :<|> WithApiKey :> "alerts" :> "top10"   :> Query (Grouped (Get '[JSON] Top10Resp))
  :<|> WithApiKey :> "alerts" :> "top10"   :> "count"    :> Query (Grouped (Get '[JSON] Top10Resp))
  :<|> WithApiKey :> "alerts" :> "top10"   :> "flapping" :> Query (Grouped (Get '[JSON] Top10Resp))

-- An environment cannot be created – it is a dynamically derived resource based on existing alerts.
type EnvironmentApi =
  WithApiKey :> "environments" :> Query (Limited (Get '[JSON] EnvironmentsResp))
--
-- A service cannot be created – it is a dynamically derived resource based on existing alerts.
type ServiceApi =
  WithApiKey :> "services" :> Query (Limited (Get '[JSON] ServicesResp))

type BlackoutApi =
      WithApiKey :> "blackout"  :> ReqBody '[JSON] Blackout :> Post '[JSON] BlackoutResp
 :<|> WithApiKey :> "blackout"  :> Capture "blackout" UUID  :> Delete '[JSON] Resp
 :<|> WithApiKey :> "blackouts" :> Get '[JSON] BlackoutsResp

type HeartbeatApi =
       WithApiKey :> "heartbeat"  :> ReqBody '[JSON] Heartbeat :> Post '[JSON] CreateHeartbeatResp
 :<|>  WithApiKey :> "heartbeat"  :> Capture "id" UUID         :> Get '[JSON] HeartbeatResp
 :<|>  WithApiKey :> "heartbeat"  :> Capture "id" UUID         :> Delete '[JSON] Resp
 :<|>  WithApiKey :> "heartbeats" :> Get '[JSON] HeartbeatsResp

type ApiKeyApi =
       NeedApiKey :> "key"  :> ReqBody '[JSON] CreateApiKey :> Post '[JSON] CreateApiKeyResp
 :<|>  WithApiKey :> "key"  :> Capture "key" ApiKey         :> Delete '[JSON] Resp
 :<|>  WithApiKey :> "keys" :> Get '[JSON] ApiKeysResp

type UserApi =
      WithApiKey :> "user"  :> ReqBody '[JSON] User           :> Post '[JSON] UserResp
 :<|> WithApiKey :> "user"  :> Capture "user" UUID            :> Delete '[JSON] Resp
 :<|> WithApiKey :> "user"  :> Capture "user" UUID            :> ReqBody '[JSON] (UserAttr 'Nonempty) :> Put '[JSON] Resp
 :<|> WithApiKey :> "users" :> QueryParam "name" CustomerName :> QueryParam "login" Email             :> Get '[JSON] UsersResp

type CustomerApi =
       WithApiKey :> "customer"  :> ReqBody '[JSON] Customer :> Post '[JSON] CustomerResp
 :<|>  WithApiKey :> "customer"  :> Capture "customer" UUID  :> Delete '[JSON] Resp
 :<|>  WithApiKey :> "customers" :> Get '[JSON] CustomersResp


--------------------------------------------------------------------------------
-- client methods
--------------------------------------------------------------------------------

createAlert           :: Maybe ApiKey -> Alert -> ClientM CreateAlertResp
getAlert              :: Maybe ApiKey -> UUID -> ClientM AlertResp
deleteAlert           :: Maybe ApiKey -> UUID -> ClientM Resp
setAlertStatus        :: Maybe ApiKey -> UUID -> StatusChange -> ClientM Resp
tagAlert              :: Maybe ApiKey -> UUID -> Tags -> ClientM Resp
untagAlert            :: Maybe ApiKey -> UUID -> Tags -> ClientM Resp
updateAlertAttributes :: Maybe ApiKey -> UUID -> Attributes -> ClientM Resp

createAlert            :<|>
 getAlert              :<|>
 deleteAlert           :<|>
 setAlertStatus        :<|>
 tagAlert              :<|>
 untagAlert            :<|>
 updateAlertAttributes = client (Proxy :: Proxy AlertApi)

listAlerts ::
     Maybe ApiKey
  -> Maybe QueryString
  -> [UUID]
  -> IsRepeat
  -> [FieldQuery]
  -> Maybe [AlertAttr]
  -> Maybe [AlertAttr]
  -> Maybe ShouldReverse
  -> [AlertAttr]
  -> Maybe PageNo
  -> Maybe Limit
  -> ClientM AlertsResp
alertHistory ::
     Maybe ApiKey
  -> Maybe QueryString
  -> [UUID]
  -> IsRepeat
  -> [FieldQuery]
  -> Maybe Limit
  -> ClientM AlertHistoryResp
countAlerts   :: Maybe ApiKey -> Maybe QueryString -> [UUID] -> IsRepeat -> [FieldQuery] ->  ClientM AlertCountResp
top10         :: Maybe ApiKey -> Maybe QueryString -> [UUID] -> IsRepeat -> [FieldQuery] -> Maybe AlertAttr -> ClientM Top10Resp
flappingTop10 :: Maybe ApiKey -> Maybe QueryString -> [UUID] -> IsRepeat -> [FieldQuery] -> Maybe AlertAttr -> ClientM Top10Resp

listAlerts    :<|>
 alertHistory :<|>
 countAlerts  :<|>
 top10        :<|>
 _            :<|>
 flappingTop10 = client (Proxy :: Proxy AlertsApi)

listEnvironments ::
  Maybe ApiKey
  -> Maybe QueryString
  -> [UUID]
  -> IsRepeat
  -> [FieldQuery]
  -> Maybe Limit
  -> ClientM EnvironmentsResp
listEnvironments = client (Proxy :: Proxy EnvironmentApi)

listServices ::
     Maybe ApiKey
  -> Maybe QueryString
  -> [UUID]
  -> IsRepeat
  -> [FieldQuery]
  -> Maybe Limit
  -> ClientM ServicesResp
listServices = client (Proxy :: Proxy ServiceApi)

createBlackout :: Maybe ApiKey -> Blackout -> ClientM BlackoutResp
deleteBlackout :: Maybe ApiKey -> UUID -> ClientM Resp
listBlackouts  :: Maybe ApiKey -> ClientM BlackoutsResp

createBlackout  :<|> deleteBlackout :<|> listBlackouts = client (Proxy :: Proxy BlackoutApi)

createHeartbeat :: Maybe ApiKey -> Heartbeat -> ClientM CreateHeartbeatResp
getHeartbeat    :: Maybe ApiKey -> UUID -> ClientM HeartbeatResp
deleteHeartbeat :: Maybe ApiKey -> UUID -> ClientM Resp
listHeartbeats  :: Maybe ApiKey -> ClientM HeartbeatsResp

createHeartbeat :<|> getHeartbeat :<|> deleteHeartbeat :<|> listHeartbeats = client (Proxy :: Proxy HeartbeatApi)

createApiKey :: ApiKey -> CreateApiKey -> ClientM CreateApiKeyResp
deleteApiKey :: Maybe ApiKey -> ApiKey -> ClientM Resp
listApiKeys  :: Maybe ApiKey -> ClientM ApiKeysResp

createApiKey :<|> deleteApiKey :<|> listApiKeys = client (Proxy :: Proxy ApiKeyApi)

createUser :: Maybe ApiKey -> User -> ClientM UserResp
deleteUser :: Maybe ApiKey -> UUID -> ClientM Resp
updateUser :: Maybe ApiKey -> UUID -> UserAttr 'Nonempty -> ClientM Resp
listUsers  :: Maybe ApiKey -> Maybe CustomerName -> Maybe Email -> ClientM UsersResp

createUser :<|> deleteUser :<|> updateUser :<|> listUsers = client (Proxy :: Proxy UserApi)

createCustomer :: Maybe ApiKey -> Customer -> ClientM CustomerResp
deleteCustomer :: Maybe ApiKey -> UUID -> ClientM Resp
listCustomers  :: Maybe ApiKey -> ClientM CustomersResp

createCustomer :<|> deleteCustomer :<|> listCustomers = client (Proxy :: Proxy CustomerApi)