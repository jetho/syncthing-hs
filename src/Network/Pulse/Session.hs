
{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Network.Pulse.Session
-- Copyright   : (c) 2014 Jens Thomas
--
-- License     : BSD-style
-- Maintainer  : jetho@gmx.de
-- Stability   : experimental
-- Portability : GHC
--
-- This module provides functions for manual session handling.
--
-- __/Example Usage:/__
--
-- @
-- \{\-\# LANGUAGE OverloadedStrings \#\-\}
--
-- import "Control.Lens" (('Control.Lens.&'), ('Control.Lens..~'))
-- import "Network.Pulse"
-- import "Network.Pulse.Session"
-- import "Network.Pulse.Get" ('Network.Pulse.Get.ping', 'Network.Pulse.Get.version')
--
-- \-\- Customized configuration.
-- settings1 = 'defaultPulseConfig' 'Control.Lens.&' 'Network.Pulse.pServer' 'Control.Lens..~' \"192.168.0.10:8080\"
--
-- session1 = do
--     session <- 'newPulseSession' settings1
--     p       <- 'runSession' session 'Network.Pulse.Get.ping'
--     v       <- 'runSession' session 'Network.Pulse.Get.version'
--     'closePulseSession' session
--     return (p, v)
--
-- \-\- Customized configuration with disabled SSL certificate verification.
-- settings2 = 'defaultPulseConfig' 'Control.Lens.&' 'Network.Pulse.pHttps'   'Control.Lens..~' True
--                                'Control.Lens.&' 'Network.Pulse.pManager' 'Control.Lens..~' Left 'Network.Pulse.noSSLVerifyManagerSettings'
--
-- session2 = do
--     session <- 'newPulseSession' settings2
--     p       <- 'runSession' session 'Network.Pulse.Get.ping'
--     v       <- 'runSession' session 'Network.Pulse.Get.version'
--     'closePulseSession' session
--     return (p, v)
-- @

module Network.Pulse.Session
    ( PulseSession
    , newPulseSession
    , closePulseSession
    , runSession
    , withSession
    ) where

import           Control.Exception   (bracket)
import           Control.Lens        ((&), (.~), (^.))
import           Network.HTTP.Client (closeManager, newManager)

import           Network.Pulse
import           Network.Pulse.Types

-- | Holds the session configuration and the connection manager.
newtype PulseSession = PulseSession { getConfig :: PulseConfig }

-- | Creates a new Pulse session for the provided configuration. You should
-- reuse the session whenever possible because of connection sharing.
newPulseSession :: PulseConfig -> IO PulseSession
newPulseSession config = do
    mgr <- createManager $ config ^. pManager
    return . PulseSession $ config & pManager .~ Right mgr
    where
        createManager (Left settings)   = newManager settings
        createManager (Right mgr)       = return mgr

-- | Closes a Pulse session.
closePulseSession :: PulseSession -> IO ()
closePulseSession session = either doNothing closeManager mgr
    where
        doNothing   = const $ return ()
        mgr         = getConfig session ^. pManager

-- | Runs a Pulse request using connection sharing within a session.
runSession :: PulseSession -> PulseM IO a -> IO (Either PulseError a)
runSession = pulse . getConfig

-- | Create a new session using the provided configuration, run the
-- action and close the session.
--
-- /Examples:/
--
-- @
-- 'withSession' defaultPulseConfig $ \\session ->
--     'runSession' session $ 'Control.Monad.liftM2' (,) 'Network.Pulse.Get.ping' 'Network.Pulse.Get.version'
-- @
-- @
-- import qualified "Network.Wreq" as Wreq
--
-- let cfg = 'defaultPulseConfig' 'Control.Lens.&' 'pHttps'  'Control.Lens..~' True
--                              'Control.Lens.&' 'pAuth'   'Control.Lens..~' Wreq.'Network.Wreq.basicAuth' \"user\" \"pass\"
--                              'Control.Lens.&' 'pApiKey' 'Control.Lens.?~' \"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\"
-- 'withSession' cfg $ \\session ->
--     'runSession' session $ 'Control.Monad.liftM2' (,) 'Network.Pulse.Get.ping' 'Network.Pulse.Get.version'
-- @
withSession :: PulseConfig -> (PulseSession -> IO (Either PulseError a)) -> IO (Either PulseError a)
withSession config = bracket (newPulseSession config) closePulseSession

