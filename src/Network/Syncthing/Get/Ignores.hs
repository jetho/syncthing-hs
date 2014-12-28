
{-# LANGUAGE OverloadedStrings #-}

module Network.Syncthing.Get.Ignores
    ( Ignore(..)
    , ignores
    ) where

import           Network.Syncthing.Common.Ignores
import           Network.Syncthing.Common.Types
import           Network.Syncthing.Query
import           Network.Syncthing.Types


ignores :: MonadSync m => FolderName -> SyncM m Ignore
ignores folder = query $ getRequest { path = "/rest/ignores"
                                    , params = [("folder", folder)]
                                    }

