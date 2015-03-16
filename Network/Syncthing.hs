
{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Network.Syncthing
-- Copyright   : (c) 2014 Jens Thomas
--
-- License     : BSD-style
-- Maintainer  : jetho@gmx.de
-- Stability   : experimental
-- Portability : GHC
--
-- Haskell bindings for the Syncthing REST API.
--
-- The library is based on the "Network.Wreq" package and uses some of wreq\'s
-- functionalities for client configuration. For example, to use authentication,
-- you need to import the "Network.Wreq" module.
--
-- __/Example Usage:/__
--
-- @
-- \{\-\# LANGUAGE OverloadedStrings \#\-\}
--
-- import qualified "Network.Wreq" as Wreq
-- import "Control.Monad" ('Control.Monad.liftM2')
-- import "Control.Lens" (('Control.Lens.&'), ('Control.Lens..~'), ('Control.Lens.?~'))
-- import "Network.Syncthing"
-- import qualified "Network.Syncthing.Get" as Get
--
-- \-\- A single Syncthing request.
-- single = 'syncthing' 'defaultConfig' Get.'Network.Syncthing.Get.ping'
--
-- \-\- Connection sharing for multiple Syncthing requests.
-- multiple1 = 'withManager' $ \\cfg ->
--     'syncthing' cfg $ do
--         p <- Get.'Network.Syncthing.Get.ping'
--         v <- Get.'Network.Syncthing.Get.version'
--         return (p, v)
--
-- \-\- Multiple Syncthing requests with connection sharing and customized configuration.
-- multiple2 = 'withManager' $ \\cfg -> do
--     let cfg\' = cfg 'Control.Lens.&' 'pServer' 'Control.Lens..~' \"192.168.0.10:8080\"
--                    'Control.Lens.&' 'pHttps'  'Control.Lens..~' True
--                    'Control.Lens.&' 'pAuth'   'Control.Lens.?~' Wreq.'Network.Wreq.basicAuth' \"user\" \"pass\"
--     'syncthing' cfg\' $ 'Control.Monad.liftM2' (,) Get.'Network.Syncthing.Get.ping' Get.'Network.Syncthing.Get.version'
-- @

module Network.Syncthing
    (
    -- * The Syncthing Monad
      SyncResult
    , SyncM
    , syncthing

    -- * Connection sharing
    , withManager
    , withManagerNoVerify
    , withManager'

    -- * Configuration
    , SyncConfig
    , pServer
    , pApiKey
    , pAuth
    , pHttps
    , pManager

    -- * Defaults
    , defaultConfig
    , defaultFolder

    -- * Manager Settings
    , defaultManagerSettings
    , noSSLVerifyManagerSettings
    , setResponseTimeout

    -- * Error Handling
    , DeviceError(..)
    , SyncError(..)

    -- * Manual Session Handling
    , module Network.Syncthing.Session

    -- * Utility functions
    , module Network.Syncthing.Utils

    -- * Types
    , module Network.Syncthing.Types
    ) where

import           Control.Lens                      (Lens', (&), (.~))
import           Data.Text                         (Text)
import           Network.Connection                (TLSSettings (..))
import qualified Network.HTTP.Client               as HTTP
import           Network.HTTP.Client.TLS           (mkManagerSettings, tlsManagerSettings)
import qualified Network.Wreq                      as W

import           Network.Syncthing.Internal.Config
import           Network.Syncthing.Internal.Error
import qualified Network.Syncthing.Internal.Lens   as PL
import           Network.Syncthing.Internal.Monad
import           Network.Syncthing.Session
import           Network.Syncthing.Types
import           Network.Syncthing.Utils


-- | Run Syncthing requests.
syncthing :: SyncConfig -> SyncM IO a -> IO (SyncResult a)
syncthing = syncthingIO

-- | Create a default configuration with a new manager for connection
-- sharing. The manager is released after running the Syncthing actions(s).
-- This is equivalent to:
--
-- @
-- 'withManager'' 'defaultManagerSettings'
-- @
--
-- /Examples:/
--
-- @
-- 'withManager' $ \\cfg ->
--     'syncthing' cfg $ 'Control.Monad.liftM2' (,) Get.'Network.Syncthing.Get.ping' Get.'Network.Syncthing.Get.version'
-- @
-- @
-- 'withManager' $ \\cfg -> do
--     let cfg\' = cfg 'Control.Lens.&' 'pServer' 'Control.Lens..~' \"192.168.0.10:8080\"
--     'syncthing' cfg\' $ 'Control.Monad.liftM2' (,) Get.'Network.Syncthing.Get.ping' Get.'Network.Syncthing.Get.version'
-- @
withManager :: (SyncConfig -> IO a) -> IO a
withManager = withManager' defaultManagerSettings

-- | Create a manager with disabled SSL certificate verification.
-- This is equivalent to:
--
-- @
-- 'withManager'' 'noSSLVerifyManagerSettings'
-- @
--
-- /Example:/
--
-- @
-- 'withManagerNoVerify' $ \\cfg -> do
--     let cfg\' = cfg 'Control.Lens.&' 'pHttps' 'Control.Lens..~' True
--     'syncthing' cfg\' $ 'Control.Monad.liftM2' (,) Get.'Network.Syncthing.Get.ping' Get.'Network.Syncthing.Get.version'
-- @
withManagerNoVerify :: (SyncConfig -> IO a) -> IO a
withManagerNoVerify = withManager' noSSLVerifyManagerSettings

-- | Create a manager by using the provided manager settings.
--
-- /Example:/
--
-- @
-- 'withManager'' 'noSSLVerifyManagerSettings' $ \\cfg -> do
--     let cfg\' = cfg 'Control.Lens.&' 'pHttps' 'Control.Lens..~' True
--     'syncthing' cfg\' $ 'Control.Monad.liftM2' (,) Get.'Network.Syncthing.Get.ping' Get.'Network.Syncthing.Get.version'
-- @
withManager' :: HTTP.ManagerSettings -> (SyncConfig -> IO a) -> IO a
withManager' settings act =
    HTTP.withManager settings $ \mgr ->
        act $ defaultConfig & pManager .~ Right mgr

-- | The default Syncthing configuration. Customize it to your needs by using
-- the SyncConfig lenses.
--
-- /Example:/
--
-- >>> defaultConfig
-- SyncConfig { pServer = "127.0.0.1:8080", pApiKey = Nothing, pAuth = Nothing, pHttps = False, pManager = Left _ }
-- >>> defaultConfig & pServer .~ "192.168.0.10:8080" & pApiKey ?~ "XXXX"
-- SyncConfig { pServer = "192.168.0.10:8080", pApiKey = Just "XXXX", pAuth = Nothing, pHttps = False, pManager = Left _ }
defaultConfig :: SyncConfig
defaultConfig = SyncConfig {
      _pServer   = "127.0.0.1:8080"
    , _pApiKey   = Nothing
    , _pAuth     = Nothing
    , _pHttps    = False
    , _pManager  = Left defaultManagerSettings
    }

-- | The default folder name.
defaultFolder :: FolderName
defaultFolder = "default"

defaultResponseTimeout :: Int
defaultResponseTimeout = 300000000

-- | Set the response timeout (in microseconds). Default is 300 seconds.
setResponseTimeout :: HTTP.ManagerSettings -> Int -> HTTP.ManagerSettings
setResponseTimeout ms t = ms { HTTP.managerResponseTimeout = Just t }

-- | The default manager settings used by 'defaultConfig'.
defaultManagerSettings :: HTTP.ManagerSettings
defaultManagerSettings =
    tlsManagerSettings `setResponseTimeout` defaultResponseTimeout

-- | Alternative manager settings with disabled SSL certificate verification.
noSSLVerifyManagerSettings :: HTTP.ManagerSettings
noSSLVerifyManagerSettings = ms `setResponseTimeout` defaultResponseTimeout
  where ms = mkManagerSettings (TLSSettingsSimple True False False) Nothing

-- | A lens for configuring the server address. Use the ADDRESS:PORT format.
--
-- /Example:/
--
-- @
-- let cfg = 'defaultConfig' 'Control.Lens.&' 'pApiKey' 'Control.Lens..~' \"192.168.0.10:8080\"
-- 'syncthing' cfg Get.'Network.Syncthing.Get.ping'
-- @
pServer :: Lens' SyncConfig Server
pServer  = PL.pServer

-- | A lens for specifying the Syncthing API Key.
--
-- /Example:/
--
-- @
-- let cfg = 'defaultConfig' 'Control.Lens.&' 'pApiKey' 'Control.Lens.?~' \"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\"
-- 'syncthing' cfg Get.'Network.Syncthing.Get.ping'
-- @
pApiKey :: Lens' SyncConfig (Maybe Text)
pApiKey  = PL.pApiKey

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
pAuth    = PL.pAuth

-- | A lens for enabling HTTPS usage.
--
-- /Example:/
--
-- @
-- let cfg = 'defaultConfig' 'Control.Lens.&' 'pHttps' 'Control.Lens..~' True
-- 'syncthing' cfg Get.'Network.Syncthing.Get.ping'
-- @
pHttps :: Lens' SyncConfig Bool
pHttps = PL.pHttps

-- | A lens for specifying your own ManagerSettings/Manager. For more
-- information, please refer to the "Network.HTTP.Client" package.
pManager :: Lens' SyncConfig (Either HTTP.ManagerSettings HTTP.Manager)
pManager = PL.pManager

