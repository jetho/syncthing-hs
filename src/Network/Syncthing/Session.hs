
{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Network.Syncthing.Session
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
-- import "Network.Syncthing"
-- import "Network.Syncthing.Session"
-- import "Network.Syncthing.Get" ('Network.Syncthing.Get.ping', 'Network.Syncthing.Get.version')
--
-- \-\- Customized configuration.
-- settings1 = 'defaultConfig' 'Control.Lens.&' 'Network.Syncthing.pServer' 'Control.Lens..~' \"192.168.0.10:8080\"
--
-- session1 = do
--     session <- 'newSyncthingSession' settings1
--     p       <- 'runSession' session 'Network.Syncthing.Get.ping'
--     v       <- 'runSession' session 'Network.Syncthing.Get.version'
--     'closeSyncthingSession' session
--     return (p, v)
--
-- \-\- Customized configuration with disabled SSL certificate verification.
-- settings2 = 'defaultConfig' 'Control.Lens.&' 'Network.Syncthing.pHttps'   'Control.Lens..~' True
--                           'Control.Lens.&' 'Network.Syncthing.pManager' 'Control.Lens..~' Left 'Network.Syncthing.noSSLVerifyManagerSettings'
--
-- session2 = do
--     session <- 'newSyncthingSession' settings2
--     p       <- 'runSession' session 'Network.Syncthing.Get.ping'
--     v       <- 'runSession' session 'Network.Syncthing.Get.version'
--     'closeSyncthingSession' session
--     return (p, v)
-- @

module Network.Syncthing.Session
    ( SyncthingSession
    , newSyncthingSession
    , closeSyncthingSession
    , runSession
    , withSession
    ) where

import           Control.Exception       (bracket)
import           Control.Lens            ((&), (.~), (^.))
import           Network.HTTP.Client     (closeManager, newManager)

import           Network.Syncthing
import           Network.Syncthing.Types

-- | Holds the session configuration and the connection manager.
newtype SyncthingSession = SyncthingSession { getConfig :: SyncthingConfig }

-- | Creates a new Syncthing session for the provided configuration. You should
-- reuse the session whenever possible because of connection sharing.
newSyncthingSession :: SyncthingConfig -> IO SyncthingSession
newSyncthingSession config = do
    mgr <- createManager $ config ^. pManager
    return . SyncthingSession $ config & pManager .~ Right mgr
    where
        createManager (Left settings)   = newManager settings
        createManager (Right mgr)       = return mgr

-- | Closes a Syncthing session.
closeSyncthingSession :: SyncthingSession -> IO ()
closeSyncthingSession session = either doNothing closeManager mgr
    where
        doNothing   = const $ return ()
        mgr         = getConfig session ^. pManager

-- | Runs a Syncthing request using connection sharing within a session.
runSession :: SyncthingSession -> SyncthingM IO a -> IO (Either SyncthingError a)
runSession = syncthing . getConfig

-- | Create a new session using the provided configuration, run the
-- action and close the session.
--
-- /Examples:/
--
-- @
-- 'withSession' defaultConfig $ \\session ->
--     'runSession' session $ 'Control.Monad.liftM2' (,) 'Network.Syncthing.Get.ping' 'Network.Syncthing.Get.version'
-- @
-- @
-- import qualified "Network.Wreq" as Wreq
--
-- let cfg = 'defaultConfig' 'Control.Lens.&' 'pHttps'  'Control.Lens..~' True
--                         'Control.Lens.&' 'pAuth'   'Control.Lens..~' Wreq.'Network.Wreq.basicAuth' \"user\" \"pass\"
--                         'Control.Lens.&' 'pApiKey' 'Control.Lens.?~' \"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\"
-- 'withSession' cfg $ \\session ->
--     'runSession' session $ 'Control.Monad.liftM2' (,) 'Network.Syncthing.Get.ping' 'Network.Syncthing.Get.version'
-- @
withSession :: SyncthingConfig -> (SyncthingSession -> IO (Either SyncthingError a)) -> IO (Either SyncthingError a)
withSession config = bracket (newSyncthingSession config) closeSyncthingSession

