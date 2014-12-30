
{-# LANGUAGE OverloadedStrings #-}

module Network.Syncthing.Get.Version
    ( Version(..)
    , version
    ) where

import           Control.Applicative              ((<$>), (<*>))
import           Control.Monad                    (MonadPlus (mzero))
import           Data.Aeson                       (FromJSON, Value (..), parseJSON, (.:))
import           Data.Text                        (Text)
import           Network.Syncthing.Internal.Query
import           Network.Syncthing.Internal.Types


data Version = Version {
      getArch        :: Text
    , getLongVersion :: Text
    , getOs          :: Text
    , getVersion     :: Text
    } deriving (Show)

version :: MonadSync m => SyncM m Version
version = query $ getRequest { path = "/rest/version" }

instance FromJSON Version where
    parseJSON (Object v) =
        Version <$> (v .: "arch")
                <*> (v .: "longVersion")
                <*> (v .: "os")
                <*> (v .: "version")
    parseJSON _          = mzero

