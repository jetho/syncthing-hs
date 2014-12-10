
{-# LANGUAGE OverloadedStrings #-}

module Network.Syncthing.Post.Error
    ( sendError
    ) where

import           Data.Aeson                    (toJSON, Value)
import           Data.Text                     (Text)
import           Network.Syncthing.Query
import           Network.Syncthing.Types


sendError :: MonadSync m => Text -> SyncM m ()
sendError msg = send $
                    SyncthingRequest {
                      path   = "/rest/error"
                    , method = Post payload
                    , params = []
                    }
  where
    payload = toJSON msg

