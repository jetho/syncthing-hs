
{-# LANGUAGE OverloadedStrings #-}

module Network.Syncthing.Common.Config
    ( Config(..)
    , FolderConfig(..)
    , DeviceConfig(..)
    , FolderDeviceConfig(..)
    , VersioningConfig(..)
    , OptionsConfig(..)
    , GuiConfig(..)
    , AddressType(..)
    ) where

import           Control.Applicative            ((<$>), (<*>))
import           Control.Monad                  (MonadPlus (mzero))
import           Data.Aeson                     (FromJSON, Value (..), parseJSON, (.:))
import           Data.Text                      (Text)

import           Network.Syncthing.Common.Types
import           Network.Syncthing.Utils


data AddressType =
      Dynamic
    | Address Addr
    deriving (Eq, Show)

data Config = Config {
      getVersion       :: Int
    , getFolderConfigs :: [FolderConfig]
    , getDeviceConfigs :: [DeviceConfig]
    , getGuiConfig     :: GuiConfig
    , getOptionsConfig :: OptionsConfig
    } deriving (Show)

data FolderConfig = FolderConfig {
      getId              :: Text
    , getPath            :: Text
    , getFolderDevices   :: [FolderDeviceConfig]
    , getReadOnly        :: Bool
    , getRescanIntervalS :: Int
    , getIgnorePerms     :: Bool
    , getVersioning      :: VersioningConfig
    , getLenientMtimes   :: Bool
    , getCopiers         :: Int
    , getPullers         :: Int
    , getFinishers       :: Int
    , getInvalid         :: Text
    } deriving (Show)

data DeviceConfig = DeviceConfig {
      getDeviceId    :: Text
    , getDeviceName  :: Text
    , getAddresses   :: [AddressType]
    , getCompression :: Bool
    , getCertName    :: Text
    , getIntroducer  :: Bool
    } deriving (Show)

data FolderDeviceConfig = FolderDeviceConfig {
      getFolderDeviceId :: Text
    } deriving (Show)

data GuiConfig = GuiConfig {
      getEnabled    :: Bool
    , getApiKey     :: Text
    , getGuiAddress :: Text
    , getUser       :: Text
    , getPassword   :: Text
    , getUseTLS     :: Bool
    } deriving (Show)

data OptionsConfig = OptionsConfig {
      getListenAddress           :: [Addr]
    , getGlobalAnnServers        :: [Text]
    , getGlobalAnnEnabled        :: Bool
    , getLocalAnnEnabled         :: Bool
    , getLocalAnnPort            :: Int
    , getLocalAnnMCAddr          :: Text
    , getMaxSendKbps             :: Int
    , getMaxRecvKbps             :: Int
    , getReconnectIntervalS      :: Int
    , getStartBrowser            :: Bool
    , getUPnPEnabled             :: Bool
    , getUPnPLease               :: Int
    , getUPnPRenewal             :: Int
    , getURAccepted              :: Int
    , getURUniqueID              :: Text
    , getRestartOnWakeup         :: Bool
    , getAutoUpgradeIntervalH    :: Int
    , getKeepTemporariesH        :: Int
    , getCacheIgnoredFiles       :: Bool
    , getProgressUpdateIntervalS :: Int
    , getSymlinksEnabled         :: Bool
} deriving (Show)

instance FromJSON Ignore where
    parseJSON (Object v) =
        Ignore <$> (v .:? "ignore")
               <*> (v .:? "patterns")
    parseJSON _          = mzero

