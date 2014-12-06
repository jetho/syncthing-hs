
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Syncthing.Get.Version
    ( Version(..)
    , version
    ) where

import           Control.Applicative     ((<$>), (<*>))
import           Control.Monad           (MonadPlus (mzero))
import           Data.Aeson
import           Data.Text
import           Network.Syncthing.Query
import           Network.Syncthing.Types


data Version = Version { 
      getArch        :: Text
    , getLongVersion :: Text
    , getOs          :: Text
    , getVersion     :: Text
    } deriving (Show)

version :: MonadST m => SyncthingM m Version
version = query $
              SyncthingRequest { 
                path   = "/rest/version"
              , method = Get
              , params = []
              }

instance FromJSON Version where
    parseJSON (Object v) =
        Version <$> (v .: "arch")
                <*> (v .: "longVersion")
                <*> (v .: "os")
                <*> (v .: "version")
    parseJSON _          = mzero

