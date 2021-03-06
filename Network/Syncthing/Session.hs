
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
-- import qualified "Network.Syncthing.Get" as Get
--
-- \-\- Customized configuration.
-- settings1 = 'defaultConfig' 'Control.Lens.&' 'Network.Syncthing.pServer' 'Control.Lens..~' \"192.168.0.10:8080\"
--
-- session1 = do
--     session <- 'newSyncSession' settings1
--     p       <- 'runSyncSession' session Get.'Network.Syncthing.Get.ping'
--     v       <- 'runSyncSession' session Get.'Network.Syncthing.Get.version'
--     'closeSyncSession' session
--     return (p, v)
--
-- \-\- Customized configuration with disabled SSL certificate verification.
-- settings2 = 'defaultConfig' 'Control.Lens.&' 'Network.Syncthing.pHttps'   'Control.Lens..~' True
--                           'Control.Lens.&' 'Network.Syncthing.pManager' 'Control.Lens..~' Left 'Network.Syncthing.noSSLVerifyManagerSettings'
--
-- session2 = do
--     session <- 'newSyncSession' settings2
--     p       <- 'runSyncSession' session Get.'Network.Syncthing.Get.ping'
--     v       <- 'runSyncSession' session Get.'Network.Syncthing.Get.version'
--     'closeSyncSession' session
--     return (p, v)
-- @

module Network.Syncthing.Session
    (
    -- * Types
      SyncSession

    -- * Session Management
    , newSyncSession
    , closeSyncSession
    , withSyncSession

    -- * Running requests
    , runSyncSession
    ) where

import           Control.Exception                 (bracket)
import           Control.Lens                      ((&), (.~), (^.))
import           Network.HTTP.Client               (closeManager, newManager)

import           Network.Syncthing.Internal.Config
import           Network.Syncthing.Internal.Monad


-- | Holds the session configuration and the connection manager.
newtype SyncSession = SyncSession { getConfig :: SyncConfig }

-- | Create a new Syncthing session for with provided configuration. You should
-- reuse the session whenever possible because of connection sharing.
newSyncSession :: SyncConfig -> IO SyncSession
newSyncSession config = do
    mgr <- createManager $ config ^. pManager
    return . SyncSession $ config & pManager .~ Right mgr
  where
    createManager (Left settings)   = newManager settings
    createManager (Right mgr)       = return mgr

-- | Close a Syncthing session.
closeSyncSession :: SyncSession -> IO ()
closeSyncSession session = either doNothing closeManager mgr
  where
    doNothing   = const $ return ()
    mgr         = getConfig session ^. pManager

-- | Run a Syncthing request using connection sharing within a session.
runSyncSession :: SyncSession -> SyncM IO a -> IO (SyncResult a)
runSyncSession = syncthingM . getConfig

-- | Create a new session using the provided configuration, run the
-- action and close the session.
--
-- /Examples:/
--
-- @
-- 'withSyncSession' 'defaultConfig' $ \\session ->
--     'runSyncSession' session $ 'Control.Monad.liftM2' (,) Get.'Network.Syncthing.Get.ping' Get.'Network.Syncthing.Get.version'
-- @
-- @
-- import qualified "Network.Wreq" as Wreq
--
-- let cfg = 'defaultConfig' 'Control.Lens.&' 'pHttps'  'Control.Lens..~' True
--                         'Control.Lens.&' 'pAuth'   'Control.Lens.?~' Wreq.'Network.Wreq.basicAuth' \"user\" \"pass\"
--                         'Control.Lens.&' 'pApiKey' 'Control.Lens.?~' \"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\"
-- 'withSyncSession' cfg $ \\session ->
--     'runSyncSession' session $ 'Control.Monad.liftM2' (,) Get.'Network.Syncthing.Get.ping' Get.'Network.Syncthing.Get.version'
-- @
withSyncSession :: SyncConfig -> (SyncSession -> IO a) -> IO a
withSyncSession config = bracket (newSyncSession config) closeSyncSession

