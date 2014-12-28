
{-# LANGUAGE OverloadedStrings #-}

module Network.Syncthing.Post.Ignores
    ( Ignores
    , sendIgnores
    ) where

import           Control.Applicative              ((<$>))
import qualified Data.Map                         as Map
import           Data.Text                        (Text)

import           Network.Syncthing.Common.Ignores
import           Network.Syncthing.Common.Types
import           Network.Syncthing.Query
import           Network.Syncthing.Types


type Ignores = [Text]

sendIgnores :: MonadSync m => FolderName -> Ignores -> SyncM m (Maybe Ignores)
sendIgnores folder ignores =
    getIgnores <$> query postRequest { path   = "/rest/ignores"
                                     , method = post ignoresMap
                                     , params = [("folder", folder)]
                                     }
  where
    ignoresMap :: Map.Map Text [Text]
    ignoresMap = Map.singleton "ignore" ignores

