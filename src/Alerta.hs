{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

--------------------------------------------------------------------------------
-- |
-- Module: Alerta
-- Description:  Bindings to the alerta API
--
-- The alerta API allows you to query and modify
--
--   * Alerts
--
--   * Environments
--
--   * Services
--
--   * Blackouts
--
--   * Heartbeats
--
--   * API Keys
--
--   * Users
--
--   * Customers
--------------------------------------------------------------------------------

module Alerta
  (
  -- * Bindings
  -- ** Alerts
    createAlert
  , getAlert
  , deleteAlert
  , setAlertStatus
  , tagAlert
  , untagAlert
  , updateAlertAttributes
  -- ** Alert history and alert queries
  , listAlerts
  , alertHistory
  , countAlerts
  , top10
  , flappingTop10
  -- ** Environments
  , listEnvironments
  -- ** Services
  , listServices
  -- ** Blackouts
  , createBlackout
  , deleteBlackout
  , listBlackouts
  -- ** Heartbeats
  , createHeartbeat
  , getHeartbeat
  , deleteHeartbeat
  , listHeartbeats
  -- ** API keys
  , createApiKey
  , deleteApiKey
  , listApiKeys
  -- ** Users
  , createUser
  , deleteUser
  , updateUser
  , listUsers
  -- ** Customers
  , createCustomer
  , deleteCustomer
  , listCustomers
  -- * Types
  , module Alerta.Types
  -- * Helpers
  , module Alerta.Helpers
  ) where

import           Alerta.Auth
import           Alerta.Helpers
import           Alerta.ServantExtras
import           Alerta.Types
import           Servant
import           Servant.Client

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
       WithApiKey :> "alert" :> ReqBody '[JSON] Alert :> Post '[JSON] (Response CreateAlertResp)
  :<|> WithApiKey :> "alert" :> Capture "id" UUID     :> Get '[JSON] (Response AlertResp)
  :<|> WithApiKey :> "alert" :> Capture "id" UUID     :> Delete '[JSON] (Response ())
  :<|> WithApiKey :> "alert" :> Capture "id" UUID     :> "status"      :> ReqBody '[JSON] StatusChange :> Put '[JSON] (Response ())
  :<|> WithApiKey :> "alert" :> Capture "id" UUID     :> "tag"         :> ReqBody '[JSON] Tags         :> Put '[JSON] (Response ())
  :<|> WithApiKey :> "alert" :> Capture "id" UUID     :> "untag"       :> ReqBody '[JSON] Tags         :> Put '[JSON] (Response ())
  :<|> WithApiKey :> "alert" :> Capture "id" UUID     :> "attributes"  :> ReqBody '[JSON] Attributes   :> Put '[JSON] (Response ())

type AlertsApi =
       WithApiKey :> "alerts" :> Query (Fields (Sort (Page (Limited (Get '[JSON] (Response AlertsResp))))))
  :<|> WithApiKey :> "alerts" :> "history" :> Query (Limited (Get '[JSON] (Response AlertHistoryResp)))
  :<|> WithApiKey :> "alerts" :> "count"   :> Query (Get '[JSON] (Response AlertCountResp))
  :<|> WithApiKey :> "alerts" :> "top10"   :> Query (Grouped (Get '[JSON] (Response Top10Resp)))
  :<|> WithApiKey :> "alerts" :> "top10"   :> "count"    :> Query (Grouped (Get '[JSON] (Response Top10Resp)))
  :<|> WithApiKey :> "alerts" :> "top10"   :> "flapping" :> Query (Grouped (Get '[JSON] (Response Top10Resp)))

-- An environment cannot be created – it is a dynamically derived resource based on existing alerts.
type EnvironmentApi =
  WithApiKey :> "environments" :> Query (Limited (Get '[JSON] (Response EnvironmentsResp)))

-- A service cannot be created – it is a dynamically derived resource based on existing alerts.
type ServiceApi =
  WithApiKey :> "services" :> Query (Limited (Get '[JSON] (Response ServicesResp)))

type BlackoutApi =
      WithApiKey :> "blackout"  :> ReqBody '[JSON] Blackout :> Post '[JSON] (Response BlackoutResp)
 :<|> WithApiKey :> "blackout"  :> Capture "blackout" UUID  :> Delete '[JSON] (Response ())
 :<|> WithApiKey :> "blackouts" :> Get '[JSON] (Response BlackoutsResp)

type HeartbeatApi =
     WithApiKey :> "heartbeat"  :> ReqBody '[JSON] Heartbeat :> Post '[JSON] (Response CreateHeartbeatResp)
 :<|>  WithApiKey :> "heartbeat"  :> Capture "id" UUID         :> Get '[JSON] (Response HeartbeatResp)
 :<|>  WithApiKey :> "heartbeat"  :> Capture "id" UUID         :> Delete '[JSON] (Response ())
 :<|>  WithApiKey :> "heartbeats" :> Get '[JSON] (Response HeartbeatsResp)

type ApiKeyApi =
       NeedApiKey :> "key"  :> ReqBody '[JSON] CreateApiKey :> Post '[JSON] (Response CreateApiKeyResp)
 :<|>  WithApiKey :> "key"  :> Capture "key" ApiKey         :> Delete '[JSON] (Response ())
 :<|>  WithApiKey :> "keys" :> Get '[JSON] (Response ApiKeysResp)

type UserApi =
      WithApiKey :> "user"  :> ReqBody '[JSON] User           :> Post '[JSON] (Response UserResp)
 :<|> WithApiKey :> "user"  :> Capture "user" UUID            :> Delete '[JSON] (Response ())
 :<|> WithApiKey :> "user"  :> Capture "user" UUID            :> ReqBody '[JSON] (UserAttr 'Nonempty) :> Put '[JSON] (Response ())
 :<|> WithApiKey :> "users" :> QueryParam "name" CustomerName :> QueryParam "login" Email             :> Get '[JSON] (Response UsersResp)

type CustomerApi =
       WithApiKey :> "customer"  :> ReqBody '[JSON] Customer :> Post '[JSON] (Response CustomerResp)
 :<|>  WithApiKey :> "customer"  :> Capture "customer" UUID  :> Delete '[JSON] (Response ())
 :<|>  WithApiKey :> "customers" :> Get '[JSON] (Response CustomersResp)


--------------------------------------------------------------------------------
-- client methods
--------------------------------------------------------------------------------

createAlert           :: Maybe ApiKey -> Alert -> ClientM (Response CreateAlertResp)
getAlert              :: Maybe ApiKey -> UUID -> ClientM (Response AlertResp)
deleteAlert           :: Maybe ApiKey -> UUID -> ClientM (Response ())
setAlertStatus        :: Maybe ApiKey -> UUID -> StatusChange -> ClientM (Response ())
tagAlert              :: Maybe ApiKey -> UUID -> Tags -> ClientM (Response ())
untagAlert            :: Maybe ApiKey -> UUID -> Tags -> ClientM (Response ())
updateAlertAttributes :: Maybe ApiKey -> UUID -> Attributes -> ClientM (Response ())

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
    -- ^ this is a JSON document describing a Mongo query
    -- see http://docs.mongodb.org/manual/reference/operator/query/
  -> [UUID]
  -> IsRepeat
  -> [FieldQuery]
  -> Maybe [AlertAttr]  -- ^ alert attributes to show
  -> Maybe [AlertAttr]  -- ^ alert attributes to hide
  -> Maybe ShouldReverse
  -> [AlertAttr]
  -> Maybe PageNo
  -> Maybe Limit
  -> ClientM (Response AlertsResp)
alertHistory ::
     Maybe ApiKey
  -> Maybe QueryString
  -> [UUID]
  -> IsRepeat
  -> [FieldQuery]
  -> Maybe Limit
  -> ClientM (Response AlertHistoryResp)
countAlerts ::
     Maybe ApiKey
  -> Maybe QueryString
  -> [UUID]
  -> IsRepeat
  -> [FieldQuery]
  -> ClientM (Response AlertCountResp)
top10 ::
     Maybe ApiKey
  -> Maybe QueryString
  -> [UUID]
  -> IsRepeat
  -> [FieldQuery]
  -> Maybe AlertAttr
  -> ClientM (Response Top10Resp)
flappingTop10 ::
     Maybe ApiKey
  -> Maybe QueryString
  -> [UUID]
  -> IsRepeat
  -> [FieldQuery]
  -> Maybe AlertAttr
  -> ClientM (Response Top10Resp)

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
  -> ClientM (Response EnvironmentsResp)
listEnvironments = client (Proxy :: Proxy EnvironmentApi)

listServices ::
     Maybe ApiKey
  -> Maybe QueryString
  -> [UUID]
  -> IsRepeat
  -> [FieldQuery]
  -> Maybe Limit
  -> ClientM (Response ServicesResp)
listServices = client (Proxy :: Proxy ServiceApi)

createBlackout :: Maybe ApiKey -> Blackout -> ClientM (Response BlackoutResp)
deleteBlackout :: Maybe ApiKey -> UUID -> ClientM (Response ())
listBlackouts  :: Maybe ApiKey -> ClientM (Response BlackoutsResp)

createBlackout  :<|> deleteBlackout :<|> listBlackouts = client (Proxy :: Proxy BlackoutApi)

createHeartbeat :: Maybe ApiKey -> Heartbeat -> ClientM (Response CreateHeartbeatResp)
getHeartbeat    :: Maybe ApiKey -> UUID -> ClientM (Response HeartbeatResp)
deleteHeartbeat :: Maybe ApiKey -> UUID -> ClientM (Response ())
listHeartbeats  :: Maybe ApiKey -> ClientM (Response HeartbeatsResp)

createHeartbeat :<|> getHeartbeat :<|> deleteHeartbeat :<|> listHeartbeats = client (Proxy :: Proxy HeartbeatApi)

createApiKey :: ApiKey -> CreateApiKey -> ClientM (Response CreateApiKeyResp)
deleteApiKey :: Maybe ApiKey -> ApiKey -> ClientM (Response ())
listApiKeys  :: Maybe ApiKey -> ClientM (Response ApiKeysResp)

createApiKey :<|> deleteApiKey :<|> listApiKeys = client (Proxy :: Proxy ApiKeyApi)

createUser :: Maybe ApiKey -> User -> ClientM (Response UserResp)
deleteUser :: Maybe ApiKey -> UUID -> ClientM (Response ())
updateUser :: Maybe ApiKey -> UUID -> UserAttr 'Nonempty -> ClientM (Response ())
listUsers  :: Maybe ApiKey -> Maybe CustomerName -> Maybe Email -> ClientM (Response UsersResp)

createUser :<|> deleteUser :<|> updateUser :<|> listUsers = client (Proxy :: Proxy UserApi)

createCustomer :: Maybe ApiKey -> Customer -> ClientM (Response CustomerResp)
deleteCustomer :: Maybe ApiKey -> UUID -> ClientM (Response ())
listCustomers  :: Maybe ApiKey -> ClientM (Response CustomersResp)

createCustomer :<|> deleteCustomer :<|> listCustomers = client (Proxy :: Proxy CustomerApi)
