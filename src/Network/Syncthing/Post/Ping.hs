
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Syncthing.Post.Ping
    ( Ping(..)
    , ping
    ) where

import           Data.Aeson
import           Network.Syncthing.Common.Ping
import           Network.Syncthing.Query
import           Network.Syncthing.Types

ping :: MonadSyncthing m => SyncthingM m Ping
ping = query $
    SyncthingRequest {
          path   = "/rest/ping"
        , method = Post payload
        , params = []
    }
    where payload = toJSON ()

