
{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Network.Pulse
-- Copyright   : (c) 2014 Jens Thomas
--
-- License     : BSD-style
-- Maintainer  : jetho@gmx.de
-- Stability   : experimental
-- Portability : GHC
--
-- Haskell bindings for the Pulse (Syncthing) REST API.
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
-- import "Network.Pulse"
-- import "Network.Pulse.Get" ('Network.Pulse.Get.ping', 'Network.Pulse.Get.version')
--
-- \-\- A single Pulse request.
-- single = 'pulse' 'defaultPulseConfig' 'Network.Pulse.Get.ping'
--
-- \-\- Using the default configuration for multiple requests is very inefficient because
-- \-\- a new connection manager gets created for each request. It's recommended to use
-- \-\- the 'withManager' function which allows connection sharing among multiple requests.
-- multiple1 = 'withManager' $ \\cfg ->
--     'pulse' cfg $ do
--         p <- 'Network.Pulse.Get.ping'
--         v <- 'Network.Pulse.Get.version'
--         return (p, v)
--
-- \-\- Multiple Pulse requests with connection sharing and customized configuration.
-- multiple2 = 'withManager' $ \\cfg -> do
--     let cfg\' = cfg 'Control.Lens.&' 'pServer' 'Control.Lens..~' \"192.168.0.10:8080\"
--                    'Control.Lens.&' 'pHttps'  'Control.Lens..~' True
--                    'Control.Lens.&' 'pAuth'   'Control.Lens..~' Wreq.'Network.Wreq.basicAuth' \"user\" \"pass\"
--     'pulse' cfg\' $ 'Control.Monad.liftM2' (,) 'Network.Pulse.Get.ping' 'Network.Pulse.Get.version'
-- @

module Network.Pulse
    (
    -- * The Pulse Monad
      PulseM
    , pulse
    -- * Multiple requests and connection sharing
    , withManager
    , withManagerNoVerify
    -- * Configuration
    , PulseConfig
    , pServer
    , pApiKey
    , pAuth
    , pHttps
    , pManager
    , defaultPulseConfig
    , defaultManagerSettings
    , noSSLVerifyManagerSettings
    -- * Error Handling
    , PulseError(..)
    ) where

import           Control.Applicative        ((<$>))
import           Control.Exception          (catch, throwIO)
import           Control.Lens               (Lens', (&), (.~), (^.))
import           Control.Monad              ((>=>))
import           Control.Monad.Trans.Either (runEitherT)
import           Control.Monad.Trans.Reader (runReaderT)
import           Data.Aeson                 (FromJSON)
import           Data.ByteString.Lazy       (fromStrict)
import           Data.Text                  (Text)
import           Network.Connection         (TLSSettings (..))
import qualified Network.HTTP.Client        as HTTP
import           Network.HTTP.Client.TLS    (mkManagerSettings, tlsManagerSettings)
import qualified Network.Wreq               as W

import qualified Network.Pulse.Lens         as PL
import           Network.Pulse.Types


-- | Creates a default configuration with a new manager for connection
-- sharing. The manager is released after running the Pulse actions(s).
--
-- /Examples:/
--
-- @
-- 'withManager' $ \\cfg ->
--     'pulse' cfg $ 'Control.Monad.liftM2' (,) 'Network.Pulse.Get.ping' 'Network.Pulse.Get.version'
-- @
-- @
-- 'withManager' $ \\cfg -> do
--     let cfg\' = cfg 'Control.Lens.&' 'pServer' 'Control.Lens..~' \"192.168.0.10:8080\"
--     'pulse' cfg\' $ 'Control.Monad.liftM2' (,) 'Network.Pulse.Get.ping' 'Network.Pulse.Get.version'
-- @
withManager :: FromJSON a => (PulseConfig -> IO (Either PulseError a)) -> IO (Either PulseError a)
withManager = withManager' defaultManagerSettings

-- | Creates a manager with disabled SSL certificate verification. 
--
-- /Example:/
--
-- @
-- 'withManagerNoVerify' $ \\cfg -> do
--     let cfg\' = cfg 'Control.Lens.&' 'pHttps' 'Control.Lens..~' True
--     'pulse' cfg\' $ 'Control.Monad.liftM2' (,) 'Network.Pulse.Get.ping' 'Network.Pulse.Get.version'
-- @
withManagerNoVerify :: FromJSON a => (PulseConfig -> IO (Either PulseError a)) -> IO (Either PulseError a)
withManagerNoVerify = withManager' noSSLVerifyManagerSettings

withManager' :: FromJSON a => HTTP.ManagerSettings -> (PulseConfig -> IO (Either PulseError a)) -> IO (Either PulseError a)
withManager' settings act =  
    HTTP.withManager settings $ \mgr ->
        act $ defaultPulseConfig & pManager .~ Right mgr

-- | Runs a single or multiple Pulse requests.
pulse :: FromJSON a => PulseConfig -> PulseM IO a -> IO (Either PulseError a)
pulse config action =
    runReaderT (runEitherT $ runPulse action) config `catch` handler
    where
        handler e@(HTTP.StatusCodeException _ headers _) =
            maybe (throwIO e) (return . Left) $ maybePulseError headers
        handler unhandledErr = throwIO unhandledErr
        maybePulseError      = lookup "X-Response-Body-Start" >=> decodeError . fromStrict

-- | The default Pulse configuration. Customize it to your needs by using
-- the PulseConfig lenses.
--
-- /Example:/
--
-- >>> defaultPulseConfig
-- PulseConfig { pServer = "127.0.0.1:8080", pApiKey = Nothing, pAuth = Nothing, pHttps = False, pManager = Left _ }
-- >>> defaultPulseConfig & pServer .~ "192.168.0.10:8080" & pApiKey ?~ "XXXX"
-- PulseConfig { pServer = "192.168.0.10:8080", pApiKey = Just "XXXX", pAuth = Nothing, pHttps = False, pManager = Left _ }
defaultPulseConfig :: PulseConfig
defaultPulseConfig = PulseConfig {
      _pServer   = "127.0.0.1:8080"
    , _pApiKey   = Nothing
    , _pAuth     = Nothing
    , _pHttps    = False
    , _pManager  = Left defaultManagerSettings
    }

-- | The default manager settings used by 'defaultPulseConfig'.
defaultManagerSettings :: HTTP.ManagerSettings
defaultManagerSettings = tlsManagerSettings

-- | Alternative manager settings with disabled SSL certificate verification.
noSSLVerifyManagerSettings :: HTTP.ManagerSettings
noSSLVerifyManagerSettings = mkManagerSettings (TLSSettingsSimple True False False) Nothing

-- | A lens for configuring the server address. Use the ADDRESS:PORT format.
--
-- /Example:/
--
-- @
-- let cfg = 'defaultPulseConfig' 'Control.Lens.&' 'pApiKey' 'Control.Lens..~' \"192.168.0.10:8080\"
-- 'pulse' cfg 'Network.Pulse.Get.ping'
-- @
pServer :: Lens' PulseConfig Text
pServer  = PL.pServer

-- | A lens for specifying the Pulse API Key.
--
-- /Example:/
--
-- @
-- let cfg = 'defaultPulseConfig' 'Control.Lens.&' 'pApiKey' 'Control.Lens.?~' \"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\"
-- 'pulse' cfg 'Network.Pulse.Get.ping'
-- @
pApiKey :: Lens' PulseConfig (Maybe Text)
pApiKey  = PL.pApiKey

-- | A lens for the authentication functionality provided by the 'Network.Wreq'
-- package (see 'Network.Wreq.Auth').
--
-- /Example:/
--
-- @
-- import qualified "Network.Wreq" as Wreq
--
-- let cfg = 'defaultPulseConfig' 'Control.Lens.&' 'pHttps' 'Control.Lens..~' True 
--                              'Control.Lens.&' 'pAuth'  'Control.Lens..~' Wreq.'Network.Wreq.basicAuth' \"user\" \"pass\"
-- 'pulse' cfg 'Network.Pulse.Get.ping'
-- @
pAuth :: Lens' PulseConfig (Maybe W.Auth)
pAuth    = PL.pAuth

-- | A lens for configuring HTTPS usage.
--
-- /Example:/
--
-- @
-- let cfg = 'defaultPulseConfig' 'Control.Lens.&' 'pHttps' 'Control.Lens..~' True
-- 'pulse' cfg 'Network.Pulse.Get.ping'
-- @
pHttps :: Lens' PulseConfig Bool
pHttps = PL.pHttps

-- | A lens for specifying your own ManagerSettings/Manager. For more
-- information, please refer to the "Network.HTTP.Client" package.
pManager :: Lens' PulseConfig (Either HTTP.ManagerSettings HTTP.Manager)
pManager = PL.pManager

-- | Use Wreq's getWith and postWith functions when running in IO
instance MonadPulse IO where
    getMethod o s    = (^. W.responseBody) <$> W.getWith o s
    postMethod o s p = (^. W.responseBody) <$> W.postWith o s p

