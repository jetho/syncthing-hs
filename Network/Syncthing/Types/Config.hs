
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Network.Syncthing.Types.Config
    ( Config(..)
    , FolderConfig(..)
    , DeviceConfig(..)
    , VersioningConfig(..)
    , OptionsConfig(..)
    , GuiConfig(..)
    , AddressType(..)
    ) where

import           Control.Applicative              ((<$>), (<*>))
import           Control.Monad                    (MonadPlus (mzero))
import           Data.Aeson
import qualified Data.Map                         as M
import           Data.Maybe                       (fromMaybe)
import           Data.Text                        (Text, cons, uncons)

import           Network.Syncthing.Types.Common
import           Network.Syncthing.Internal.Utils



-------------------------------------------------------------------------------
-- CONFIG RECORD -----
-------------------------------------------------------------------------------

-- | The current configuration data structure.
data Config = Config {
      getConfigVersion :: Int
    , getFolderConfigs :: [FolderConfig]
    , getDeviceConfigs :: [DeviceConfig]
    , getGuiConfig     :: GuiConfig
    , getOptionsConfig :: OptionsConfig
    } deriving (Eq, Show)

instance FromJSON Config where
    parseJSON (Object v) =
        Config <$> (v .: "version")
               <*> (v .: "folders")
               <*> (v .: "devices")
               <*> (v .: "gui")
               <*> (v .: "options")
    parseJSON _          = mzero

instance ToJSON Config where
    toJSON Config{..} =
        object [ "version"  .= getConfigVersion
               , "folders"  .= getFolderConfigs
               , "devices"  .= getDeviceConfigs
               , "gui"      .= getGuiConfig
               , "options"  .= getOptionsConfig
               ]


-------------------------------------------------------------------------------
-- ADDRESS TYPE -----
-------------------------------------------------------------------------------

-- | An address can be dynamic or static.
data AddressType =
      Dynamic
    | Address Addr
    deriving (Eq, Show)

decodeAddressType :: Text -> AddressType
decodeAddressType "dynamic" = Dynamic
decodeAddressType addr      = Address $ parseAddr addr

encodeAddressType :: AddressType -> Text
encodeAddressType Dynamic        = "dynamic"
encodeAddressType (Address addr) = encodeAddr addr


-------------------------------------------------------------------------------
-- FOLDER CONFIG -----
-------------------------------------------------------------------------------

-- | The folder specific configuration.
data FolderConfig = FolderConfig {
      getId              :: FolderName
    , getPath            :: Path
    , getFolderDevices   :: [Device]
    , getReadOnly        :: Bool
    , getRescanIntervalS :: Int
    , getIgnorePerms     :: Bool
    , getAutoNormalize   :: Bool
    , getVersioning      :: VersioningConfig
    , getCopiers         :: Int
    , getPullers         :: Int
    , getHashers         :: Int
    , getOrder           :: Text
    , getFolderInvalid   :: Text
    } deriving (Eq, Show)

instance FromJSON FolderConfig where
    parseJSON (Object v) =
        FolderConfig <$> (v .: "id")
                     <*> (v .: "path")
                     <*> (map getFolderDevice <$> (v .: "devices"))
                     <*> (v .: "readOnly")
                     <*> (v .: "rescanIntervalS")
                     <*> (v .: "ignorePerms")
                     <*> (v .: "autoNormalize")
                     <*> (v .: "versioning")
                     <*> (v .: "copiers")
                     <*> (v .: "pullers")
                     <*> (v .: "hashers")
                     <*> (v .: "order")
                     <*> (v .: "invalid")
    parseJSON _          = mzero

instance ToJSON FolderConfig where
    toJSON FolderConfig{..} =
        object [ "id"              .= getId
               , "path"            .= getPath
               , "devices"         .= map FolderDeviceConfig getFolderDevices
               , "readOnly"        .= getReadOnly
               , "rescanIntervalS" .= getRescanIntervalS
               , "ignorePerms"     .= getIgnorePerms
               , "autoNormalize"   .= getAutoNormalize
               , "versioning"      .= getVersioning
               , "copiers"         .= getCopiers
               , "pullers"         .= getPullers
               , "hashers"         .= getHashers
               , "order"           .= getOrder
               , "invalid"         .= getFolderInvalid
               ]


-------------------------------------------------------------------------------
-- VERSIONING CONFIG -----
-------------------------------------------------------------------------------

-- | Information about versioning.
data VersioningConfig = VersioningConfig {
      getType   :: Text
    , getParams :: M.Map Text Text
    } deriving (Eq, Show)

instance FromJSON VersioningConfig where
    parseJSON (Object v) =
        VersioningConfig <$> (v .: "type")
                         <*> (v .: "params")
    parseJSON _          = mzero

instance ToJSON VersioningConfig where
    toJSON VersioningConfig{..} =
        object [ "type"   .= getType
               , "params" .= getParams
               ]


-------------------------------------------------------------------------------
-- DEVICE CONFIG -----
-------------------------------------------------------------------------------

-- | Device specific configuration information.
data DeviceConfig = DeviceConfig {
      getDevice      :: Device
    , getDeviceName  :: Text
    , getAddresses   :: [AddressType]
    , getCompression :: Text
    , getCertName    :: Text
    , getIntroducer  :: Bool
    } deriving (Eq, Show)

instance FromJSON DeviceConfig where
    parseJSON (Object v) =
        DeviceConfig <$> (v .: "deviceID")
                     <*> (v .: "name")
                     <*> (map decodeAddressType <$> (v .: "addresses"))
                     <*> (v .: "compression")
                     <*> (v .: "certName")
                     <*> (v .: "introducer")
    parseJSON _          = mzero

instance ToJSON DeviceConfig where
    toJSON DeviceConfig{..} =
        object [ "deviceID"     .= getDevice
               , "name"         .= getDeviceName
               , "addresses"    .= map encodeAddressType getAddresses
               , "compression"  .= getCompression
               , "certName"     .= getCertName
               , "introducer"   .= getIntroducer
               ]


-------------------------------------------------------------------------------
-- FOLDER-DEVICE CONFIG -----
-------------------------------------------------------------------------------

data FolderDeviceConfig = FolderDeviceConfig {
      getFolderDevice :: Device
    } deriving (Eq, Show)

instance FromJSON FolderDeviceConfig where
    parseJSON (Object v) = FolderDeviceConfig <$> (v .: "deviceID")
    parseJSON _          = mzero

instance ToJSON FolderDeviceConfig where
    toJSON (FolderDeviceConfig device) =
        object [ "deviceID" .= device ]


-------------------------------------------------------------------------------
-- GUI CONFIG -----
-------------------------------------------------------------------------------

-- | Gui settings.
data GuiConfig = GuiConfig {
      getEnabled    :: Bool
    , getApiKey     :: Maybe Text
    , getGuiAddress :: Addr
    , getUser       :: Text
    , getPassword   :: Text
    , getUseTLS     :: Bool
    } deriving (Eq, Show)

instance FromJSON GuiConfig where
    parseJSON (Object v) =
        GuiConfig <$> (v .: "enabled")
                  <*> (decodeApiKey <$> (v .: "apiKey"))
                  <*> (parseAddr <$> (v .: "address"))
                  <*> (v .: "user")
                  <*> (v .: "password")
                  <*> (v .: "useTLS")
    parseJSON _          = mzero

instance ToJSON GuiConfig where
    toJSON GuiConfig{..} =
        object [ "enabled"  .= getEnabled
               , "apiKey"   .= encodeApiKey getApiKey
               , "address"  .= encodeAddr getGuiAddress
               , "user"     .= getUser
               , "password" .= getPassword
               , "useTLS"   .= getUseTLS
               ]

decodeApiKey :: Text -> Maybe Text
decodeApiKey = (uncurry cons `fmap`) . uncons

encodeApiKey :: Maybe Text -> Text
encodeApiKey = fromMaybe ""


-------------------------------------------------------------------------------
-- OPTIONS CONFIG -----
-------------------------------------------------------------------------------

-- | Various config settings.
data OptionsConfig = OptionsConfig {
      getListenAddress           :: [Addr]
    , getGlobalAnnounceServers   :: [Text]
    , getGlobalAnnounceEnabled   :: Bool
    , getLocalAnnounceEnabled    :: Bool
    , getLocalAnnouncePort       :: Int
    , getLocalAnnounceMCAddr     :: Text
    , getMaxSendKbps             :: Int
    , getMaxRecvKbps             :: Int
    , getReconnectionIntervalS   :: Int
    , getStartBrowser            :: Bool
    , getUpnpEnabled             :: Bool
    , getUpnpLeaseMinutes        :: Int
    , getUpnpRenewalMinutes      :: Int
    , getUpnpTimeoutSeconds      :: Int
    , getUrAccepted              :: Int
    , getUrUniqueID              :: Text
    , getRestartOnWakeup         :: Bool
    , getAutoUpgradeIntervalH    :: Int
    , getKeepTemporariesH        :: Int
    , getCacheIgnoredFiles       :: Bool
    , getProgressUpdateIntervalS :: Int
    , getSymlinksEnabled         :: Bool
    , getLimitBandwidthInLan     :: Bool
} deriving (Eq, Show)

instance FromJSON OptionsConfig where
    parseJSON (Object v) =
        OptionsConfig <$> (map parseAddr <$> (v .: "listenAddress"))
                      <*> (v .: "globalAnnounceServers")
                      <*> (v .: "globalAnnounceEnabled")
                      <*> (v .: "localAnnounceEnabled")
                      <*> (v .: "localAnnouncePort")
                      <*> (v .: "localAnnounceMCAddr")
                      <*> (v .: "maxSendKbps")
                      <*> (v .: "maxRecvKbps")
                      <*> (v .: "reconnectionIntervalS")
                      <*> (v .: "startBrowser")
                      <*> (v .: "upnpEnabled")
                      <*> (v .: "upnpLeaseMinutes")
                      <*> (v .: "upnpRenewalMinutes")
                      <*> (v .: "upnpTimeoutSeconds")
                      <*> (v .: "urAccepted")
                      <*> (v .: "urUniqueId")
                      <*> (v .: "restartOnWakeup")
                      <*> (v .: "autoUpgradeIntervalH")
                      <*> (v .: "keepTemporariesH")
                      <*> (v .: "cacheIgnoredFiles")
                      <*> (v .: "progressUpdateIntervalS")
                      <*> (v .: "symlinksEnabled")
                      <*> (v .: "limitBandwidthInLan")
    parseJSON _          = mzero

instance ToJSON OptionsConfig where
    toJSON OptionsConfig{..} =
        object [ "listenAddress"           .= map encodeAddr getListenAddress
               , "globalAnnounceServers"   .= getGlobalAnnounceServers
               , "globalAnnounceEnabled"   .= getGlobalAnnounceEnabled
               , "localAnnounceEnabled"    .= getLocalAnnounceEnabled
               , "localAnnouncePort"       .= getLocalAnnouncePort
               , "localAnnounceMCAddr"     .= getLocalAnnounceMCAddr
               , "maxSendKbps"             .= getMaxSendKbps
               , "maxRecvKbps"             .= getMaxRecvKbps
               , "reconnectionIntervalS"   .= getReconnectionIntervalS
               , "startBrowser"            .= getStartBrowser
               , "upnpEnabled"             .= getUpnpEnabled
               , "upnpLeaseMinutes"        .= getUpnpLeaseMinutes
               , "upnpRenewalMinutes"      .= getUpnpRenewalMinutes
               , "upnpTimeoutSeconds"      .= getUpnpTimeoutSeconds
               , "urAccepted"              .= getUrAccepted
               , "urUniqueId"              .= getUrUniqueID
               , "restartOnWakeup"         .= getRestartOnWakeup
               , "autoUpgradeIntervalH"    .= getAutoUpgradeIntervalH
               , "keepTemporariesH"        .= getKeepTemporariesH
               , "cacheIgnoredFiles"       .= getCacheIgnoredFiles
               , "progressUpdateIntervalS" .= getProgressUpdateIntervalS
               , "symlinksEnabled"         .= getSymlinksEnabled
               , "limitBandwidthInLan"     .= getLimitBandwidthInLan
               ]

