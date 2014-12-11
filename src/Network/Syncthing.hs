
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
-- import "Control.Lens" (('Control.Lens.&'), ('Control.Lens..~'))
-- import "Network.Syncthing"
-- import "Network.Syncthing.Get" ('Network.Syncthing.Get.ping', 'Network.Syncthing.Get.version')
--
-- \-\- A single Syncthing request.
-- single = 'syncthing' 'defaultConfig' 'Network.Syncthing.Get.ping'
--
-- \-\- Using the default configuration for multiple requests is very inefficient because
-- \-\- a new connection manager gets created for each request. It's recommended to use
-- \-\- the 'withManager' function which allows connection sharing among multiple requests.
-- multiple1 = 'withManager' $ \\cfg ->
--     'syncthing' cfg $ do
--         p <- 'Network.Syncthing.Get.ping'
--         v <- 'Network.Syncthing.Get.version'
--         return (p, v)
--
-- \-\- Multiple Syncthing requests with connection sharing and customized configuration.
-- multiple2 = 'withManager' $ \\cfg -> do
--     let cfg\' = cfg 'Control.Lens.&' 'pServer' 'Control.Lens..~' \"192.168.0.10:8080\"
--                    'Control.Lens.&' 'pHttps'  'Control.Lens..~' True
--                    'Control.Lens.&' 'pAuth'   'Control.Lens..~' Wreq.'Network.Wreq.basicAuth' \"user\" \"pass\"
--     'syncthing' cfg\' $ 'Control.Monad.liftM2' (,) 'Network.Syncthing.Get.ping' 'Network.Syncthing.Get.version'
-- @

module Network.Syncthing
    (
    -- * Types
      SyncResult

    -- * The Syncthing Monad
    , SyncM
    , syncthing

    -- * Multiple requests and connection sharing
    , withManager
    , withManagerNoVerify

    -- * Configuration
    , SyncConfig
    , pServer
    , pApiKey
    , pAuth
    , pHttps
    , pManager
    , defaultConfig

    -- * Manager Settings
    , defaultManagerSettings
    , noSSLVerifyManagerSettings

    -- * Error Handling
    , DeviceError(..)
    , SyncError(..)
    ) where

import           Control.Applicative        ((<$>))
import           Control.Exception          (catch, throwIO)
import           Control.Lens               (Lens', (&), (.~), (^.))
import           Control.Monad              ((<=<))
import           Control.Monad.Trans.Either (runEitherT)
import           Control.Monad.Trans.Reader (runReaderT)
import           Data.ByteString.Lazy       (fromStrict)
import           Data.Text                  (Text)
import           Network.Connection         (TLSSettings (..))
import qualified Network.HTTP.Client        as HTTP
import           Network.HTTP.Client.TLS    (mkManagerSettings,
                                             tlsManagerSettings)
import qualified Network.Wreq               as W

import qualified Network.Syncthing.Lens     as PL
import           Network.Syncthing.Types
import           Network.Syncthing.Utils    (decodeError)


-- | Use Wreq's getWith and postWith functions when running in IO
instance MonadSync IO where
    getMethod  o s   = (^. W.responseBody) <$> W.getWith  o s
    postMethod o s p = (^. W.responseBody) <$> W.postWith o s p

-- | Creates a default configuration with a new manager for connection
-- sharing. The manager is released after running the Syncthing actions(s).
--
-- /Examples:/
--
-- @
-- 'withManager' $ \\cfg ->
--     'syncthing' cfg $ 'Control.Monad.liftM2' (,) 'Network.Syncthing.Get.ping' 'Network.Syncthing.Get.version'
-- @
-- @
-- 'withManager' $ \\cfg -> do
--     let cfg\' = cfg 'Control.Lens.&' 'pServer' 'Control.Lens..~' \"192.168.0.10:8080\"
--     'syncthing' cfg\' $ 'Control.Monad.liftM2' (,) 'Network.Syncthing.Get.ping' 'Network.Syncthing.Get.version'
-- @
withManager :: (SyncConfig -> IO (SyncResult a)) -> IO (SyncResult a)
withManager = withManager' defaultManagerSettings

-- | Creates a manager with disabled SSL certificate verification.
--
-- /Example:/
--
-- @
-- 'withManagerNoVerify' $ \\cfg -> do
--     let cfg\' = cfg 'Control.Lens.&' 'pHttps' 'Control.Lens..~' True
--     'syncthing' cfg\' $ 'Control.Monad.liftM2' (,) 'Network.Syncthing.Get.ping' 'Network.Syncthing.Get.version'
-- @
withManagerNoVerify :: (SyncConfig -> IO (SyncResult a)) -> IO (SyncResult a) 
withManagerNoVerify = withManager' noSSLVerifyManagerSettings

withManager' :: HTTP.ManagerSettings -> (SyncConfig -> IO (SyncResult a)) -> IO (SyncResult a)
withManager' settings act =
    HTTP.withManager settings $ \mgr ->
        act $ defaultConfig & pManager .~ Right mgr

-- | Runs a single or multiple Syncthing requests.
syncthing :: SyncConfig -> SyncM IO a -> IO (SyncResult a)
syncthing config action =
    runReaderT (runEitherT $ runSyncthing action) config `catch` handler
  where
    handler e@(HTTP.StatusCodeException _ headers _) =
        maybe (throwIO e) (return . Left) $ maybeSyncError headers
    handler unhandledErr = throwIO unhandledErr
    maybeSyncError = decodeError . fromStrict <=< lookup "X-Response-Body-Start" 

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

-- | The default manager settings used by 'defaultConfig'.
defaultManagerSettings :: HTTP.ManagerSettings
defaultManagerSettings = tlsManagerSettings

-- | Alternative manager settings with disabled SSL certificate verification.
noSSLVerifyManagerSettings :: HTTP.ManagerSettings
noSSLVerifyManagerSettings = 
    mkManagerSettings (TLSSettingsSimple True False False) Nothing

-- | A lens for configuring the server address. Use the ADDRESS:PORT format.
--
-- /Example:/
--
-- @
-- let cfg = 'defaultConfig' 'Control.Lens.&' 'pApiKey' 'Control.Lens..~' \"192.168.0.10:8080\"
-- 'syncthing' cfg 'Network.Syncthing.Get.ping'
-- @
pServer :: Lens' SyncConfig Text
pServer  = PL.pServer

-- | A lens for specifying the Syncthing API Key.
--
-- /Example:/
--
-- @
-- let cfg = 'defaultConfig' 'Control.Lens.&' 'pApiKey' 'Control.Lens.?~' \"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\"
-- 'syncthing' cfg 'Network.Syncthing.Get.ping'
-- @
pApiKey :: Lens' SyncConfig (Maybe Text)
pApiKey  = PL.pApiKey

-- | A lens for the authentication functionality provided by the 'Network.Wreq'
-- package (see 'Network.Wreq.Auth').
--
-- /Example:/
--
-- @
-- import qualified "Network.Wreq" as Wreq
--
-- let cfg = 'defaultConfig' 'Control.Lens.&' 'pHttps' 'Control.Lens..~' True
--                         'Control.Lens.&' 'pAuth'  'Control.Lens..~' Wreq.'Network.Wreq.basicAuth' \"user\" \"pass\"
-- 'syncthing' cfg 'Network.Syncthing.Get.ping'
-- @
pAuth :: Lens' SyncConfig (Maybe W.Auth)
pAuth    = PL.pAuth

-- | A lens for configuring HTTPS usage.
--
-- /Example:/
--
-- @
-- let cfg = 'defaultConfig' 'Control.Lens.&' 'pHttps' 'Control.Lens..~' True
-- 'syncthing' cfg 'Network.Syncthing.Get.ping'
-- @
pHttps :: Lens' SyncConfig Bool
pHttps = PL.pHttps

-- | A lens for specifying your own ManagerSettings/Manager. For more
-- information, please refer to the "Network.HTTP.Client" package.
pManager :: Lens' SyncConfig (Either HTTP.ManagerSettings HTTP.Manager)
pManager = PL.pManager

