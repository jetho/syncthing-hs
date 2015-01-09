
{-# LANGUAGE OverloadedStrings #-}

module Network.Syncthing.Get.Need
    ( need 
    ) where

import           Network.Syncthing.Common.Need
import           Network.Syncthing.Common.Types
import           Network.Syncthing.Internal.Query
import           Network.Syncthing.Internal.Types


need :: MonadSync m => FolderName -> SyncM m Need
need folder = query $ getRequest { path   = "/rest/need"
                                 , params = [("folder", folder)]
                                 }

