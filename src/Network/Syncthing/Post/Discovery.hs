
{-# LANGUAGE OverloadedStrings #-}

module Network.Syncthing.Post.Discovery
    ( hint
    ) where

import           Network.Syncthing.Common.Types
import           Network.Syncthing.Internal.Query
import           Network.Syncthing.Internal.Types


hint:: MonadSync m => DeviceId -> Server -> SyncM m ()
hint device server=
    send $ postRequest { path   = "/rest/discovery/hint"
                       , params = [("device", device), ("addr", server)]
                       }

