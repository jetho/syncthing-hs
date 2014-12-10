
{-# LANGUAGE OverloadedStrings #-}

module Network.Syncthing.Post.System
    ( restart
    , shutdown
    , reset
    ) where

import           Network.Syncthing.Query
import           Network.Syncthing.Types


restart :: MonadSync m => SyncM m ()
restart = send $ postRequest { path = "/rest/restart" }

shutdown :: MonadSync m => SyncM m ()
shutdown = send $ postRequest { path = "/rest/shutdown" }

reset :: MonadSync m => SyncM m ()
reset = send $ postRequest { path = "/rest/reset" }
