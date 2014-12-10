
{-# LANGUAGE OverloadedStrings #-}

module Network.Syncthing.Post.Error
    ( sendError
    , clearErrors
    ) where

import           Data.Text                     (Text)
import           Network.Syncthing.Query
import           Network.Syncthing.Types


sendError :: MonadSync m => Text -> SyncM m ()
sendError msg = send $ postRequest { path   = "/rest/error"
                                   , method = post msg 
                                   }

clearErrors :: MonadSync m => SyncM m ()
clearErrors = send $ postRequest { path = "/rest/error/clear" } 
