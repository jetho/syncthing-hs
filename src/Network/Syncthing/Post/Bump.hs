
{-# LANGUAGE OverloadedStrings #-}

module Network.Syncthing.Post.Bump
    ( bump
    ) where

import           Network.Syncthing.Common.Need
import           Network.Syncthing.Common.Types
import           Network.Syncthing.Internal.Query
import           Network.Syncthing.Internal.Types


bump :: MonadSync m => FolderName -> Path -> SyncM m Need
bump folder filePath =
    query $ postRequest { path   = "/rest/bump"
                        , params = [ ("folder", folder) , ("file", filePath) ]
                        }

