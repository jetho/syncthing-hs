
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      : Network.Syncthing.Post
-- Copyright   : (c) 2014 Jens Thomas
--
-- License     : BSD-style
-- Maintainer  : jetho@gmx.de
-- Stability   : experimental
-- Portability : GHC
--
-- Syncthing POST requests.

module Network.Syncthing.Post
    (
    -- * System services
      config
    , discovery
    , ping
    , sendError
    , clearErrors
    , reset
    , restart
    , shutdown
    , upgrade

    -- * Database Services
    , ignores
    , prio
    , scan
    ) where

import           Control.Applicative                ((<$>))
import           Control.Monad                      (join, (>=>))
import qualified Data.Map                           as Map
import           Data.Maybe                         (catMaybes, maybeToList)
import           Data.Text                          (Text, pack)

import           Network.Syncthing.Internal.Monad
import           Network.Syncthing.Internal.Request
import           Network.Syncthing.Internal.Types


maybeSystemMsg :: MonadSync m => SyncRequest -> SyncM m (Maybe SystemMsg)
maybeSystemMsg = queryMaybe >=> return . join

-- | Ping the Syncthing server. Returns the string \"pong\".
ping :: MonadSync m => SyncM m Text
ping = getPing <$> query postRequest { path = "/rest/system/ping" }

-- | Move the given file to the top of the download queue.
prio :: MonadSync m => FolderName -> Path -> SyncM m Need
prio folder filePath =
    query postRequest { path   = "/rest/db/prio"
                      , params = [ ("folder", folder) , ("file", filePath) ]
                      }

-- | Add an entry to the discovery cache.
discovery :: MonadSync m => Device -> Server -> SyncM m ()
discovery  device server =
    send postRequest { path   = "/rest/system/discovery"
                     , params = [("device", device), ("addr", server)]
                     }

-- | Update the server configuration. The configuration will be saved to
-- disk and the configInSync flag set to false. 
-- 'Network.Syncthing.Post.restart' Syncthing to activate.
config :: MonadSync m => Config -> SyncM m ()
config cfg = send postRequest { path   = "/rest/system/config"
                              , method = post cfg
                              }

-- | Register a new error message.
sendError :: MonadSync m => Text -> SyncM m ()
sendError msg = send postRequest { path   = "/rest/system/error"
                                 , method = post msg
                                 }

-- | Remove all recent errors.
clearErrors :: MonadSync m => SyncM m ()
clearErrors = send $ postRequest { path = "/rest/system/error/clear" }

-- | Update the ignores list and echo it back as response.
ignores :: MonadSync m => FolderName -> [Text] -> SyncM m (Maybe [Text])
ignores folder ignoresList =
    getIgnores <$> query postRequest { path   = "/rest/db/ignores"
                                     , method = post ignoresMap
                                     , params = [("folder", folder)]
                                     }
  where
    ignoresMap :: Map.Map Text [Text]
    ignoresMap = Map.singleton "ignore" ignoresList

-- | Request rescan of a folder. Restrict the scan to a relative subpath
-- within the folder by specifying the optional path parameter. The
-- optional int argument delays Syncthing's automated rescan interval for a 
-- given amount of seconds.
scan:: MonadSync m => FolderName -> Maybe Path -> Maybe Int-> SyncM m ()
scan folder subPath next =
    send postRequest { path   = "/rest/db/scan"
                     , params = ("folder", folder) : optionalParams
                     }
  where 
    optionalParams = catMaybes [ ("sub",) <$> subPath
                               , ("next",) . pack . show <$> next
                               ]

-- | Restart Syncthing.
restart :: MonadSync m => SyncM m SystemMsg
restart = query postRequest { path = "/rest/system/restart" }

-- | Shutdown Syncthing.
shutdown :: MonadSync m => SyncM m SystemMsg
shutdown = query postRequest { path = "/rest/system/shutdown" }

-- | Erase the current index database and restart Syncthing. Erase the
-- entire database or just a specific folder by specifying the folder parameter.
reset :: MonadSync m => Maybe FolderName -> SyncM m SystemMsg
reset folder = 
    query postRequest { path   = "/rest/system/reset" 
                      , params = maybeToList $ ("folder",) <$> folder
                      }

-- | Perform an upgrade to the newest release and restart. Does nothing if
-- there is no newer version.
upgrade :: MonadSync m => SyncM m (Maybe SystemMsg)
upgrade = maybeSystemMsg postRequest { path = "/rest/system/upgrade" }

