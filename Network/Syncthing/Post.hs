
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
-- The POST requests.

module Network.Syncthing.Post
    ( 
    -- * Request functions
      ping
    , bump
    , hint
    , sendConfig
    , sendError
    , clearErrors
    , sendIgnores
    , scanFolder
    , reset
    , restart
    , shutdown
    , upgrade
    ) where

import           Control.Applicative              ((<$>))
import           Control.Monad                    (join, (>=>))
import qualified Data.Map                         as Map
import           Data.Maybe                       (maybeToList)
import           Data.Text                        (Text)

import           Network.Syncthing
import           Network.Syncthing.Internal.Monad
import           Network.Syncthing.Internal.Request


querySystemMsg :: MonadSync m => SyncRequest -> SyncM m (Maybe SystemMsg)
querySystemMsg = queryMaybe >=> return . join

-- | Ping the Syncthing server. Returns the string \"pong\".
ping :: MonadSync m => SyncM m Text
ping = getPing <$> ping'
  where
    ping' = query $ postRequest { path = "/rest/ping" }

-- | Moves the given file to the top of the download queue.
bump :: MonadSync m => FolderName -> Path -> SyncM m Need
bump folder filePath =
    query $ postRequest { path   = "/rest/bump"
                        , params = [ ("folder", folder) , ("file", filePath) ]
                        }

-- | Add an entry to the discovery cache.
hint:: MonadSync m => Device -> Server -> SyncM m ()
hint device server=
    send $ postRequest { path   = "/rest/discovery/hint"
                       , params = [("device", device), ("addr", server)]
                       }
 
-- | Update the server configuration. The configuration will be saved to
-- disk and the configInSync flag set to false. 'Network.Syncthing.Post.restart' Syncthing to
-- activate.
sendConfig :: MonadSync m => Config -> SyncM m ()
sendConfig cfg = send $ postRequest { path   = "/rest/config"
                                     , method = post cfg
                                     }

-- | Register a new error message.
sendError :: MonadSync m => Text -> SyncM m ()
sendError msg = send $ postRequest { path   = "/rest/error"
                                   , method = post msg
                                   }

-- | Remove all recent errors.
clearErrors :: MonadSync m => SyncM m ()
clearErrors = send $ postRequest { path = "/rest/error/clear" }

-- | Update the ignores list and echo it back as response.
sendIgnores :: MonadSync m => FolderName -> [Text] -> SyncM m (Maybe [Text])
sendIgnores folder ignores =
    getIgnores <$> query postRequest { path   = "/rest/ignores"
                                     , method = post ignoresMap
                                     , params = [("folder", folder)]
                                     }
  where
    ignoresMap :: Map.Map Text [Text]
    ignoresMap = Map.singleton "ignore" ignores

-- | Request rescan of a folder. Restrict the scan to a relative subpath
-- within the folder by specifying the optional path parameter.
scanFolder:: MonadSync m => FolderName -> Maybe Path -> SyncM m ()
scanFolder folder subPath =
    send $ postRequest { path   = "/rest/scan"
                       , params = [("folder", folder)]
                                  ++ maybeToList (("sub",) <$> subPath)
                       }

-- | Restart Syncthing.
restart :: MonadSync m => SyncM m (Maybe SystemMsg)
restart = querySystemMsg $ postRequest { path = "/rest/restart" }

-- | Shutdown Syncthing.
shutdown :: MonadSync m => SyncM m (Maybe SystemMsg)
shutdown = querySystemMsg $ postRequest { path = "/rest/shutdown" }

-- | Reset Syncthing by renaming all folder directories to temporary,
-- unique names, wiping all indexes and restarting.
reset :: MonadSync m => SyncM m (Maybe SystemMsg)
reset = querySystemMsg $ postRequest { path = "/rest/reset" }

-- | Perform an upgrade to the newest release and restart. Does nothing if
-- there is no newer version.
upgrade :: MonadSync m => SyncM m (Maybe SystemMsg)
upgrade = querySystemMsg $ postRequest { path = "/rest/upgrade" }

