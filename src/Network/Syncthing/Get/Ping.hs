
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Syncthing.Get.Ping
    ( Ping(..)
    , ping
    ) where

import           Network.Syncthing.Common.Ping
import           Network.Syncthing.Query
import           Network.Syncthing.Types


ping :: MonadST m => SyncthingM m Ping
ping = query $
    SyncthingRequest {
          path   = "/rest/ping"
        , method = Get
        , params = []
    }

