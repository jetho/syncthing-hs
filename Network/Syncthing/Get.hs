
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      : Network.Syncthing.Get
-- Copyright   : (c) 2014 Jens Thomas
--
-- License     : BSD-style
-- Maintainer  : jetho@gmx.de
-- Stability   : experimental
-- Portability : GHC
--
-- The GET requests.

module Network.Syncthing.Get
    (
    -- * Request functions
      ping
    , apiKey
    , config
    , completion
    , connections
    , deviceId
    , discovery
    , errors
    , ignores
    , model
    , need
    , sync
    , system
    , upgrade
    , version
    ) where

import           Control.Applicative                ((<$>))
import           Control.Monad                      ((>=>))
import qualified Data.Map                           as M
import           Data.Text                          (Text)

import           Network.Syncthing
import           Network.Syncthing.Internal.Monad
import           Network.Syncthing.Internal.Request
import           Network.Syncthing.Types.Error      (getErrors)
import           Network.Syncthing.Types.DeviceId   ()


-- | Ping the Syncthing server. Returns the string \"pong\".
ping :: MonadSync m => SyncM m Text
ping = getPing <$> ping'
  where
    ping' = query $ getRequest { path = "/rest/ping" }

-- | Returns the current configuration.
config :: MonadSync m => SyncM m Config
config = query $ getRequest { path = "/rest/config" }

-- | Get the API Key if available.
apiKey :: MonadSync m => SyncM m (Maybe Text)
apiKey = getApiKey . getGuiConfig <$> config

-- | Returns the completion percentage (0 to 100) for a given device and
-- folder. 
completion :: MonadSync m => Device -> FolderName -> SyncM m Int
completion device folder = getCompletion <$> completion'
  where
    completion' = query $ getRequest { path   = "/rest/completion"
                                     , params = [ ("device", device)
                                                , ("folder", folder) ]
                                     }

-- | Returns the list of current connections and some metadata associated
-- with the connection/peer.
connections :: MonadSync m => SyncM m (M.Map Device Connection)
connections = query $ getRequest { path = "/rest/connections" }

-- | Verifies and formats a device ID. Returns either a valid device ID in
-- modern format, or an error.
deviceId :: MonadSync m => Device -> SyncM m Device
deviceId = deviceId' >=> either (liftLeft . InvalidDeviceId) liftRight
  where
    deviceId' :: MonadSync m => Device -> SyncM m (Either DeviceError Device)
    deviceId' device = query $ getRequest { path   = "/rest/deviceid"
                                          , params = [("id", device)]
                                          }

-- | Returns the contents of the local discovery cache.
discovery :: MonadSync m => SyncM m (M.Map Device [CacheEntry])
discovery = query $ getRequest { path = "/rest/discovery" }

-- | Returns the list of recent errors.
errors :: MonadSync m => SyncM m [Error]
errors = getErrors <$> errors'
  where
    errors' = query $ getRequest { path = "/rest/errors" }

-- | Returns the ignores list.
ignores :: MonadSync m => FolderName -> SyncM m Ignore
ignores folder = query $ getRequest { path   = "/rest/ignores"
                                    , params = [ ("folder", folder) ]
                                    }

-- | Returns information about the current status of a folder.
model :: MonadSync m => FolderName -> SyncM m Model
model folder = query $ getRequest { path   = "/rest/model"
                                  , params = [("folder", folder)]
                                  }

-- | Returns lists of files which are needed by this device in order for it
-- to become in sync. 
need :: MonadSync m => FolderName -> SyncM m Need
need folder = query $ getRequest { path   = "/rest/need"
                                 , params = [ ("folder", folder) ]
                                 }

-- | Returns whether the config is in sync.
sync :: MonadSync m => SyncM m Bool
sync = getSync <$> sync'
  where
    sync' = query $ getRequest { path = "/rest/config/sync" }

-- | Returns information about current system status and resource usage.
system :: MonadSync m => SyncM m System
system = query $ getRequest { path = "/rest/system" }

-- | Checks for a possible upgrade.
upgrade :: MonadSync m => SyncM m Upgrade
upgrade = query $ getRequest { path   = "/rest/upgrade" }

-- | Returns the current syncthing version information.
version :: MonadSync m => SyncM m Version
version = query $ getRequest { path = "/rest/version" }

