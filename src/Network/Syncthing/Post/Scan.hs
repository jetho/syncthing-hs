
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Network.Syncthing.Post.Scan
    ( scanFolder
    ) where

import           Control.Applicative            ((<$>))
import           Data.Maybe                     (maybeToList)
import           Data.Text                      ()
import           Network.Syncthing.Common.Types
import           Network.Syncthing.Query
import           Network.Syncthing.Types


scanFolder:: MonadSync m => FolderName -> Maybe SubPath -> SyncM m ()
scanFolder folder subPath =
    send $ postRequest { path   = "/rest/scan"
                       , params = ("folder", folder) : maybeToList maybeSubPath
                       }
  where
    maybeSubPath = ("sub",) <$> subPath

