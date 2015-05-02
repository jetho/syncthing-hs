
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
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
-- Syncthing GET requests.

module Network.Syncthing.Get
    (
    -- * System Services
      ping
    , apiKey
    , config
    , insync
    , connections
    , discovery
    , errors
    , sysStatus
    , upgrade
    , version

    -- * Database Services
    , browse
    , completion
    , file
    , ignores
    , need
    , dbStatus

    -- * Statistics Services
    , devices
    --, folder 

    -- * Miscellaneous Services
    , deviceId
    , lang
    , report
    ) where

import           Control.Applicative                ((<$>))
import           Control.Monad                      ((>=>))
import qualified Data.Map                           as M
import           Data.Maybe                         (catMaybes)
import           Data.Text                          (Text, pack)

import           Network.Syncthing.Internal.Error
import           Network.Syncthing.Internal.Monad
import           Network.Syncthing.Internal.Request
import           Network.Syncthing.Internal.Types


-- | Ping the Syncthing server. Returns the string \"pong\".
ping :: MonadSync m => SyncM m Text
ping = getPing <$> query getRequest { path = "/rest/system/ping" }

-- | Return the current configuration.
config :: MonadSync m => SyncM m Config
config = query getRequest { path = "/rest/system/config" }

-- | Get the API Key if available.
apiKey :: MonadSync m => SyncM m (Maybe Text)
apiKey = getApiKey . getGuiConfig <$> config

-- | Return the completion percentage (0 to 100) for a given device and
-- folder.
completion :: MonadSync m => Device -> FolderName -> SyncM m Int
completion device folder = 
    getCompletion <$> query getRequest { path   = "/rest/db/completion"
                                       , params = [ ("device", device)
                                                  , ("folder", folder) ]
                                       }

-- | Get the list of current connections and some metadata associated
-- with the connection/peer.
connections :: MonadSync m => SyncM m Connections
connections = query getRequest { path = "/rest/system/connections" }

-- | Returns most data available about a given file, including version and
-- availability.
file :: MonadSync m => FolderName -> Path -> SyncM m DBFile
file folder filename = query getRequest { path   = "/rest/db/file"
                                        , params = [ ("folder", folder)
                                                   , ("file", filename) ]
                                        }

-- | Verifiy and format a device ID. Return either a valid device ID in
-- modern format, or an error.
deviceId :: MonadSync m => Device -> SyncM m Device
deviceId = deviceId' >=> either (liftLeft . InvalidDeviceId) liftRight
  where
    deviceId' :: MonadSync m => Device -> SyncM m (Either DeviceError Device)
    deviceId' device = query getRequest { path   = "/rest/svc/deviceid"
                                        , params = [("id", device)]
                                        }

-- | Returns general statistics about devices.
devices :: MonadSync m => SyncM m (M.Map Device DeviceInfo)
devices = query getRequest { path = "/rest/stats/device" }

-- | Fetch the contents of the local discovery cache.
discovery :: MonadSync m => SyncM m (M.Map Device [CacheEntry])
discovery = query getRequest { path = "/rest/system/discovery" }

-- | Get the list of recent errors.
errors :: MonadSync m => SyncM m [Error]
errors = getErrors <$> query getRequest { path = "/rest/system/error" }

-- | Fetch the ignores list.
ignores :: MonadSync m => FolderName -> SyncM m Ignore
ignores folder = query getRequest { path   = "/rest/db/ignores"
                                  , params = [("folder", folder)]
                                  }

-- | Get information about the current status of a folder.
dbStatus :: MonadSync m => FolderName -> SyncM m Model
dbStatus folder = query getRequest { path   = "/rest/db/status"
                                   , params = [("folder", folder)]
                                   }

-- | Get lists of files which are needed by this device in order for it
-- to become in sync.
need :: MonadSync m => FolderName -> SyncM m Need
need folder = query getRequest { path   = "/rest/db/need"
                               , params = [("folder", folder)]
                               }

-- | Returns the data sent in the anonymous usage report.
report :: MonadSync m => SyncM m UsageReport
report = query getRequest { path   = "/rest/svc/report" }

-- | Determine whether the config is in sync.
insync :: MonadSync m => SyncM m Bool
insync = getSync <$> query getRequest { path = "/rest/system/config/insync" }

-- | Returns information about current system status and resource usage.
sysStatus :: MonadSync m => SyncM m System
sysStatus = query getRequest { path = "/rest/system/status" }

-- | Get the directory tree of the global model.
browse :: MonadSync m 
    => FolderName -- ^ root folder
    -> Maybe Path -- ^ defines a prefix within the tree where to start building the structure
    -> Maybe Int  -- ^ defines how deep within the tree we want to dwell down (0 based, defaults to unlimited depth)  
    -> SyncM m (Maybe DirTree)
browse folder prefix levels  = 
    queryMaybe getRequest { path   = "/rest/db/browse"
                          , params = [("folder", folder)] ++ optionals
                          }
  where 
    optionals  = catMaybes [("prefix",) <$> prefix, ("levels",) <$> levelsText]
    levelsText = pack . show <$> levels

-- | Check for a possible upgrade.
upgrade :: MonadSync m => SyncM m Upgrade
upgrade = query getRequest { path   = "/rest/system/upgrade" }

-- | Get the current syncthing version information.
version :: MonadSync m => SyncM m Version
version = query getRequest { path = "/rest/system/version" }

-- | Returns a list of canonicalized localization codes, as picked up from
-- the Accept-Language header sent by the browser.
lang :: MonadSync m => SyncM m [Text]
lang = query getRequest { path = "/rest/svc/lang" }

