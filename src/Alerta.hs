{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds   #-}

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
  , module Alerta.Response
  , module Control.Monad.Trans.Response
  -- * Helpers
  , module Alerta.Helpers
  ) where

import           Alerta.Auth
import           Alerta.Helpers
import           Alerta.Response
import           Alerta.ServantExtras
import           Alerta.Types
import           Control.Monad.Trans.Response
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

createAlert           :: Maybe ApiKey -> Alert -> ResponseT ClientM CreateAlertResp
getAlert              :: Maybe ApiKey -> UUID -> ResponseT ClientM AlertResp
deleteAlert           :: Maybe ApiKey -> UUID -> ResponseT ClientM ()
setAlertStatus        :: Maybe ApiKey -> UUID -> StatusChange -> ResponseT ClientM ()
tagAlert              :: Maybe ApiKey -> UUID -> Tags -> ResponseT ClientM ()
untagAlert            :: Maybe ApiKey -> UUID -> Tags -> ResponseT ClientM ()
updateAlertAttributes :: Maybe ApiKey -> UUID -> Attributes -> ResponseT ClientM ()

createAlert           = raise2 createAlert'
getAlert              = raise2 getAlert'
deleteAlert           = raise2 deleteAlert'
setAlertStatus        = raise3 setAlertStatus'
tagAlert              = raise3 tagAlert'
untagAlert            = raise3 untagAlert'
updateAlertAttributes = raise3 updateAlertAttributes'

createAlert'            :<|>
 getAlert'              :<|>
 deleteAlert'           :<|>
 setAlertStatus'        :<|>
 tagAlert'              :<|>
 untagAlert'            :<|>
 updateAlertAttributes' = client (Proxy :: Proxy AlertApi)

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
  -> ResponseT ClientM AlertsResp
alertHistory ::
     Maybe ApiKey
  -> Maybe QueryString
  -> [UUID]
  -> IsRepeat
  -> [FieldQuery]
  -> Maybe Limit
  -> ResponseT ClientM AlertHistoryResp
countAlerts ::
     Maybe ApiKey
  -> Maybe QueryString
  -> [UUID]
  -> IsRepeat
  -> [FieldQuery]
  -> ResponseT ClientM AlertCountResp
top10 ::
     Maybe ApiKey
  -> Maybe QueryString
  -> [UUID]
  -> IsRepeat
  -> [FieldQuery]
  -> Maybe AlertAttr
  -> ResponseT ClientM Top10Resp
flappingTop10 ::
     Maybe ApiKey
  -> Maybe QueryString
  -> [UUID]
  -> IsRepeat
  -> [FieldQuery]
  -> Maybe AlertAttr
  -> ResponseT ClientM Top10Resp

listAlerts    = raise11 listAlerts'
alertHistory  = raise6 alertHistory'
countAlerts   = raise5 countAlerts'
top10         = raise6 top10'
flappingTop10 = raise6 flappingTop10'

listAlerts'    :<|>
 alertHistory' :<|>
 countAlerts'  :<|>
 top10'        :<|>
 _             :<|>
 flappingTop10' = client (Proxy :: Proxy AlertsApi)

listEnvironments ::
  Maybe ApiKey
  -> Maybe QueryString
  -> [UUID]
  -> IsRepeat
  -> [FieldQuery]
  -> Maybe Limit
  -> ResponseT ClientM EnvironmentsResp
listEnvironments = raise6 listEnvironments'

listEnvironments' = client (Proxy :: Proxy EnvironmentApi)

listServices ::
     Maybe ApiKey
  -> Maybe QueryString
  -> [UUID]
  -> IsRepeat
  -> [FieldQuery]
  -> Maybe Limit
  -> ResponseT ClientM ServicesResp
listServices = raise6 listServices'

listServices' = client (Proxy :: Proxy ServiceApi)

createBlackout :: Maybe ApiKey -> Blackout -> ResponseT ClientM BlackoutResp
deleteBlackout :: Maybe ApiKey -> UUID -> ResponseT ClientM ()
listBlackouts  :: Maybe ApiKey -> ResponseT ClientM BlackoutsResp

createBlackout = raise2 createBlackout'
deleteBlackout = raise2 deleteBlackout'
listBlackouts  = raise1 listBlackouts'

createBlackout' :<|> deleteBlackout' :<|> listBlackouts' = client (Proxy :: Proxy BlackoutApi)

createHeartbeat :: Maybe ApiKey -> Heartbeat -> ResponseT ClientM CreateHeartbeatResp
getHeartbeat    :: Maybe ApiKey -> UUID -> ResponseT ClientM HeartbeatResp
deleteHeartbeat :: Maybe ApiKey -> UUID -> ResponseT ClientM ()
listHeartbeats  :: Maybe ApiKey -> ResponseT ClientM HeartbeatsResp

createHeartbeat = raise2 createHeartbeat'
getHeartbeat    = raise2 getHeartbeat'
deleteHeartbeat = raise2 deleteHeartbeat'
listHeartbeats  = raise1 listHeartbeats'

createHeartbeat' :<|> getHeartbeat' :<|> deleteHeartbeat' :<|> listHeartbeats' = client (Proxy :: Proxy HeartbeatApi)

createApiKey :: ApiKey -> CreateApiKey -> ResponseT ClientM CreateApiKeyResp
deleteApiKey :: Maybe ApiKey -> ApiKey -> ResponseT ClientM ()
listApiKeys  :: Maybe ApiKey -> ResponseT ClientM ApiKeysResp

createApiKey = raise2 createApiKey'
deleteApiKey = raise2 deleteApiKey'
listApiKeys  = raise1 listApiKeys'

createApiKey' :<|> deleteApiKey' :<|> listApiKeys' = client (Proxy :: Proxy ApiKeyApi)

createUser :: Maybe ApiKey -> User -> ResponseT ClientM UserResp
deleteUser :: Maybe ApiKey -> UUID -> ResponseT ClientM ()
updateUser :: Maybe ApiKey -> UUID -> UserAttr 'Nonempty -> ResponseT ClientM ()
listUsers  :: Maybe ApiKey -> Maybe CustomerName -> Maybe Email -> ResponseT ClientM UsersResp

createUser = raise2 createUser'
deleteUser = raise2 deleteUser'
updateUser = raise3 updateUser'
listUsers  = raise3 listUsers'

createUser' :<|> deleteUser' :<|> updateUser' :<|> listUsers' = client (Proxy :: Proxy UserApi)

createCustomer :: Maybe ApiKey -> Customer -> ResponseT ClientM CustomerResp
deleteCustomer :: Maybe ApiKey -> UUID -> ResponseT ClientM ()
listCustomers  :: Maybe ApiKey -> ResponseT ClientM CustomersResp

createCustomer = raise2 createCustomer'
deleteCustomer = raise2 deleteCustomer'
listCustomers  = raise1 listCustomers'

createCustomer' :<|> deleteCustomer' :<|> listCustomers' = client (Proxy :: Proxy CustomerApi)

compose1  :: (r -> s) -> (a -> r) -> (a -> s)
compose2  :: (r -> s) -> (a -> b -> r) -> (a -> b -> s)
compose3  :: (r -> s) -> (a -> b -> c -> r) -> (a -> b -> c -> s)
compose4  :: (r -> s) -> (a -> b -> c -> d -> r) -> (a -> b -> c -> d -> s)
compose5  :: (r -> s) -> (a -> b -> c -> d -> e -> r) -> (a -> b -> c -> d -> e -> s)
compose6  :: (r -> s) -> (a -> b -> c -> d -> e -> f -> r) -> (a -> b -> c -> d -> e -> f -> s)
compose7  :: (r -> s) -> (a -> b -> c -> d -> e -> f -> g -> r) -> (a -> b -> c -> d -> e -> f -> g -> s)
compose8  :: (r -> s) -> (a -> b -> c -> d -> e -> f -> g -> h -> r) -> (a -> b -> c -> d -> e -> f -> g -> h -> s)
compose9  :: (r -> s) -> (a -> b -> c -> d -> e -> f -> g -> h -> i -> r) -> (a -> b -> c -> d -> e -> f -> g -> h -> i -> s)
compose10 :: (r -> s) -> (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> r) -> (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> s)
compose11 :: (r -> s) -> (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> r) -> (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> s)

compose1  = (.)
compose2  = compose1  . (.)
compose3  = compose2  . (.)
compose4  = compose3  . (.)
compose5  = compose4  . (.)
compose6  = compose5  . (.)
compose7  = compose6  . (.)
compose8  = compose7  . (.)
compose9  = compose8  . (.)
compose10 = compose9  . (.)
compose11 = compose10 . (.)

raise1  :: (a -> m (Response r)) -> a -> ResponseT m r
raise2  :: (a -> b -> m (Response r)) -> a -> b -> ResponseT m r
raise3  :: (a -> b -> c -> m (Response r)) -> a -> b -> c -> ResponseT m r
raise4  :: (a -> b -> c -> d -> m (Response r)) -> a -> b -> c -> d -> ResponseT m r
raise5  :: (a -> b -> c -> d -> e -> m (Response r)) -> a -> b -> c -> d -> e -> ResponseT m r
raise6  :: (a -> b -> c -> d -> e -> f -> m (Response r)) -> a -> b -> c -> d -> e -> f -> ResponseT m r
raise7  :: (a -> b -> c -> d -> e -> f -> g -> m (Response r)) -> a -> b -> c -> d -> e -> f -> g -> ResponseT m r
raise8  :: (a -> b -> c -> d -> e -> f -> g -> h -> m (Response r)) -> a -> b -> c -> d -> e -> f -> g -> h -> ResponseT m r
raise9  :: (a -> b -> c -> d -> e -> f -> g -> h -> i -> m (Response r)) -> a -> b -> c -> d -> e -> f -> g -> h -> i -> ResponseT m r
raise10 :: (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> m (Response r)) -> a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> ResponseT m r
raise11 :: (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> m (Response r)) -> a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> ResponseT m r

raise1  = compose1 ResponseT
raise2  = compose2 ResponseT
raise3  = compose3 ResponseT
raise4  = compose4 ResponseT
raise5  = compose5 ResponseT
raise6  = compose6 ResponseT
raise7  = compose7 ResponseT
raise8  = compose8 ResponseT
raise9  = compose9 ResponseT
raise10 = compose10 ResponseT
raise11 = compose11 ResponseT
