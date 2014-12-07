
{-# LANGUAGE OverloadedStrings #-}

module Network.Syncthing.Get.Sync
    ( sync
    ) where

import           Control.Applicative     ((<$>))
import           Control.Monad           (MonadPlus (mzero))
import           Data.Aeson
import           Network.Syncthing.Query
import           Network.Syncthing.Types


data Sync = Sync { 
      getConfigInSync :: Bool
    } deriving (Show)

sync :: MonadST m => SyncthingM m Bool
sync = getConfigInSync <$> sync'
  where
    sync' = query $
                SyncthingRequest { 
                  path   = "/rest/config/sync"
                , method = Get
                , params = []
                }

instance FromJSON Sync where
    parseJSON (Object v) = Sync <$> (v .: "configInSync")
    parseJSON _          = mzero
