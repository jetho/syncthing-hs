
{-# LANGUAGE OverloadedStrings #-}

module Network.Syncthing.Post.Discovery
    ( hint
    ) where

import           Network.Syncthing.Common.Types
import           Network.Syncthing.Query
import           Network.Syncthing.Types


hint:: MonadSync m => DeviceId -> Addr -> SyncM m ()
hint device addr =
    send $ postRequest { path   = "/rest/discovery/hint"
                       , params = [("device", device), ("addr", addr)]
                       }

