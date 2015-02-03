
{-# LANGUAGE OverloadedStrings #-}

module Network.Syncthing.Types.Config
    ( Config(..)
    , FolderConfig(..)
    , DeviceConfig(..)
    , VersioningConfig(..)
    , OptionsConfig(..)
    , GuiConfig(..)
    , AddressType(..)
    ) where

import           Control.Applicative            ((<$>), (<*>))
import           Control.Monad                  (MonadPlus (mzero))
import           Data.Aeson                     (FromJSON, Value (..), parseJSON, (.:))
import qualified Data.Map                       as M
import           Data.Text                      (Text, cons, uncons)

import           Network.Syncthing.Types.Common
import           Network.Syncthing.Utils


-- | An address can be dynamic or static.
data AddressType =
      Dynamic
    | Address Addr
    deriving (Eq, Show)

-- | The current configuration data structure.
data Config = Config {
      getConfigVersion :: Int
    , getFolderConfigs :: [FolderConfig]
    , getDeviceConfigs :: [DeviceConfig]
    , getGuiConfig     :: GuiConfig
    , getOptionsConfig :: OptionsConfig
    } deriving (Show)

-- | The folder specific configuration.
data FolderConfig = FolderConfig {
      getId              :: FolderName
    , getPath            :: Path
    , getFolderDevices   :: [DeviceId]
    , getReadOnly        :: Bool
    , getRescanIntervalS :: Int
    , getIgnorePerms     :: Bool
    , getVersioning      :: VersioningConfig
    , getLenientMtimes   :: Bool
    , getCopiers         :: Int
    , getPullers         :: Int
    , getFinishers       :: Int
    , getFolderInvalid   :: Text
    } deriving (Show)

-- | Information about versioning.
data VersioningConfig = VersioningConfig {
      getType   :: Text
    , getParams :: M.Map Text Text
    } deriving Show

-- | Device specific configuration information.
data DeviceConfig = DeviceConfig {
      getDeviceId    :: DeviceId
    , getDeviceName  :: Text
    , getAddresses   :: [AddressType]
    , getCompression :: Bool
    , getCertName    :: Text
    , getIntroducer  :: Bool
    } deriving (Show)

data FolderDeviceConfig = FolderDeviceConfig {
      getFolderDeviceId :: DeviceId
    } deriving (Show)

-- | Gui settings.
data GuiConfig = GuiConfig {
      getEnabled    :: Bool
    , getApiKey     :: Maybe Text
    , getGuiAddress :: Addr
    , getUser       :: Text
    , getPassword   :: Text
    , getUseTLS     :: Bool
    } deriving (Show)

-- | Various config settings.  
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


instance FromJSON Config where
    parseJSON (Object v) =
        Config <$> (v .: "Version")
               <*> (v .: "Folders")
               <*> (v .: "Devices")
               <*> (v .: "GUI")
               <*> (v .: "Options")
    parseJSON _          = mzero

instance FromJSON FolderConfig where
    parseJSON (Object v) =
        FolderConfig <$> (v .: "ID")
                     <*> (v .: "Path")
                     <*> (map getFolderDeviceId <$> (v .: "Devices"))
                     <*> (v .: "ReadOnly")
                     <*> (v .: "RescanIntervalS")
                     <*> (v .: "IgnorePerms")
                     <*> (v .: "Versioning")
                     <*> (v .: "LenientMtimes")
                     <*> (v .: "Copiers")
                     <*> (v .: "Pullers")
                     <*> (v .: "Finishers")
                     <*> (v .: "Invalid")
    parseJSON _          = mzero

instance FromJSON VersioningConfig where
    parseJSON (Object v) =
        VersioningConfig <$> (v .: "Type")
                         <*> (v .: "Params")
    parseJSON _          = mzero

instance FromJSON DeviceConfig where
    parseJSON (Object v) =
        DeviceConfig <$> (v .: "DeviceID")
                     <*> (v .: "Name")
                     <*> (map decodeAddressType <$> (v .: "Addresses"))
                     <*> (v .: "Compression")
                     <*> (v .: "CertName")
                     <*> (v .: "Introducer")
    parseJSON _          = mzero

decodeAddressType :: Text -> AddressType
decodeAddressType "dynamic" = Dynamic
decodeAddressType addr      = Address $ parseAddr addr

instance FromJSON FolderDeviceConfig where
    parseJSON (Object v) = FolderDeviceConfig <$> (v .: "DeviceID")
    parseJSON _          = mzero

instance FromJSON GuiConfig where
    parseJSON (Object v) =
        GuiConfig <$> (v .: "Enabled")
                  <*> (decodeApiKey <$> (v .: "APIKey"))
                  <*> (parseAddr <$> (v .: "Address"))
                  <*> (v .: "User")
                  <*> (v .: "Password")
                  <*> (v .: "UseTLS")
    parseJSON _          = mzero

decodeApiKey :: Text -> Maybe Text
decodeApiKey = (uncurry cons `fmap`) . uncons 

instance FromJSON OptionsConfig where
    parseJSON (Object v) =
        OptionsConfig <$> (map parseAddr <$> (v .: "ListenAddress"))
                      <*> (v .: "GlobalAnnServers")
                      <*> (v .: "GlobalAnnEnabled")
                      <*> (v .: "LocalAnnEnabled")
                      <*> (v .: "LocalAnnPort")
                      <*> (v .: "LocalAnnMCAddr")
                      <*> (v .: "MaxSendKbps")
                      <*> (v .: "MaxRecvKbps")
                      <*> (v .: "ReconnectIntervalS")
                      <*> (v .: "StartBrowser")
                      <*> (v .: "UPnPEnabled")
                      <*> (v .: "UPnPLease")
                      <*> (v .: "UPnPRenewal")
                      <*> (v .: "URAccepted")
                      <*> (v .: "URUniqueID")
                      <*> (v .: "RestartOnWakeup")
                      <*> (v .: "AutoUpgradeIntervalH")
                      <*> (v .: "KeepTemporariesH")
                      <*> (v .: "CacheIgnoredFiles")
                      <*> (v .: "ProgressUpdateIntervalS")
                      <*> (v .: "SymlinksEnabled")
    parseJSON _          = mzero

