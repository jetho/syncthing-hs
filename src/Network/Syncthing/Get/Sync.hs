
{-# LANGUAGE OverloadedStrings #-}

module Network.Syncthing.Get.Sync
    ( sync
    ) where

import           Control.Applicative     ((<$>))
import           Control.Monad           (MonadPlus (mzero))
import           Data.Aeson              (FromJSON, Value (..), parseJSON, (.:))
import           Network.Syncthing.Query
import           Network.Syncthing.Types


data Sync = Sync {
      getConfigInSync :: Bool
    } deriving (Show)

sync :: MonadSync m => SyncM m Bool
sync = getConfigInSync <$> sync'
  where
    sync' = query $ getRequest { path = "/rest/config/sync" }

instance FromJSON Sync where
    parseJSON (Object v) = Sync <$> (v .: "configInSync")
    parseJSON _          = mzero

