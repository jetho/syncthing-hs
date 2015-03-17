
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Network.Syncthing.Internal.Config
    ( SyncConfig(..)
    , pServer
    , pApiKey
    , pAuth
    , pHttps
    , pManager
    ) where

import           Control.Lens                   (Lens', lens)
import qualified Data.Text                      as T
import           Network.HTTP.Client            (Manager, ManagerSettings)
import qualified Network.Wreq                   as W

import           Network.Syncthing.Types.Common (Server)


-- | The Syncthing configuration for specifying the Syncthing server,
-- authentication, the API Key etc.
data SyncConfig = SyncConfig {
      server    :: Server
    , apiKey    :: Maybe T.Text
    , auth      :: Maybe W.Auth
    , https     :: Bool
    , manager   :: Either ManagerSettings Manager
    }

instance Show SyncConfig where
    show SyncConfig{..} =
        concat [ "SyncConfig { "
               , "server = ", show server
               , ", apiKey = ", show apiKey
               , ", auth = ", show auth
               , ", https = ", show https
               , ", manager = ", case manager of
                      Left _  -> "Left _"
                      Right _ -> "Right _"
               , " }"
               ]

-- | A lens for configuring the server address. Use the ADDRESS:PORT format.
--
-- /Example:/
--
-- @
-- let cfg = 'defaultConfig' 'Control.Lens.&' 'pServer' 'Control.Lens..~' \"192.168.0.10:8080\"
-- 'syncthing' cfg Get.'Network.Syncthing.Get.ping'
-- @
pServer :: Lens' SyncConfig Server
pServer = lens server (\f new -> f { server = new })

-- | A lens for specifying the Syncthing API Key.
--
-- /Example:/
--
-- @
-- let cfg = 'defaultConfig' 'Control.Lens.&' 'pApiKey' 'Control.Lens.?~' \"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\"
-- 'syncthing' cfg Get.'Network.Syncthing.Get.ping'
-- @
pApiKey :: Lens' SyncConfig (Maybe T.Text)
pApiKey = lens apiKey (\f new -> f { apiKey = new })

-- | A lens for configuring request authentication provided by the
-- 'Network.Wreq' package (see 'Network.Wreq.Auth').
--
-- /Example:/
--
-- @
-- import qualified "Network.Wreq" as Wreq
--
-- let cfg = 'defaultConfig' 'Control.Lens.&' 'pHttps' 'Control.Lens..~' True
--                         'Control.Lens.&' 'pAuth'  'Control.Lens.?~' Wreq.'Network.Wreq.basicAuth' \"user\" \"pass\"
-- 'syncthing' cfg Get.'Network.Syncthing.Get.ping'
-- @
pAuth :: Lens' SyncConfig (Maybe W.Auth)
pAuth = lens auth (\f new -> f { auth = new })

-- | A lens for enabling HTTPS usage.
--
-- /Example:/
--
-- @
-- let cfg = 'defaultConfig' 'Control.Lens.&' 'pHttps' 'Control.Lens..~' True
-- 'syncthing' cfg Get.'Network.Syncthing.Get.ping'
-- @
pHttps :: Lens' SyncConfig Bool
pHttps = lens https (\f new -> f { https = new })

-- | A lens for specifying your own ManagerSettings/Manager. For more
-- information, please refer to the "Network.HTTP.Client" package.
pManager :: Lens' SyncConfig (Either ManagerSettings Manager)
pManager = lens manager (\f new -> f { manager = new })

