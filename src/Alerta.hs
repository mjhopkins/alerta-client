{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Alerta where

import           Alerta.Types
import           Servant
import           Servant.Client

type AlertApi
  =    "alert" :> ReqBody '[JSON] Alert :> Post '[JSON] CreateAlertResp
  :<|> "alert" :> Capture "id" UUID     :> Get '[JSON] AlertResp
  :<|> "alert" :> Capture "id" UUID     :> Delete '[JSON] Resp
  :<|> "alert" :> Capture "id" UUID     :> "status"      :> ReqBody '[JSON] StatusChange :> Put '[JSON] Resp
  :<|> "alert" :> Capture "id" UUID     :> "tag"         :> ReqBody '[JSON] Tags         :> Put '[JSON] Resp
  :<|> "alert" :> Capture "id" UUID     :> "untag"       :> ReqBody '[JSON] Tags         :> Put '[JSON] Resp
  :<|> "alert" :> Capture "id" UUID     :> "attributes"  :> ReqBody '[JSON] Attributes   :> Put '[JSON] Resp

createAlert
 :<|> getAlert
 :<|> deleteAlert
 :<|> setAlertStatus
 :<|> tagAlert
 :<|> untagAlert
 :<|> updateAlertAttributes
 = client (Proxy :: Proxy AlertApi)