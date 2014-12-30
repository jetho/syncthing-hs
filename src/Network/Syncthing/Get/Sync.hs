
{-# LANGUAGE OverloadedStrings #-}

module Network.Syncthing.Get.Sync
    ( sync
    ) where

import           Control.Applicative              ((<$>))
import           Control.Monad                    (MonadPlus (mzero))
import           Data.Aeson                       (FromJSON, Value (..), parseJSON, (.:))
import           Network.Syncthing.Internal.Query
import           Network.Syncthing.Internal.Types


newtype Sync = Sync { getSync :: Bool } deriving (Show)

sync :: MonadSync m => SyncM m Bool
sync = getSync <$> sync'
  where
    sync' = query $ getRequest { path = "/rest/config/sync" }

instance FromJSON Sync where
    parseJSON (Object v) = Sync <$> (v .: "configInSync")
    parseJSON _          = mzero

