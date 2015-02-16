
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

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
import           Data.Aeson                     
import qualified Data.Map                       as M
import           Data.Maybe                     (fromMaybe)
import           Data.Text                      (Text, cons, uncons)

import           Network.Syncthing.Types.Common
import           Network.Syncthing.Utils



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
    } deriving (Show)

instance FromJSON Config where
    parseJSON (Object v) =
        Config <$> (v .: "Version")
               <*> (v .: "Folders")
               <*> (v .: "Devices")
               <*> (v .: "GUI")
               <*> (v .: "Options")
    parseJSON _          = mzero

instance ToJSON Config where
    toJSON Config {..} =
        object [ "Version"  .= getConfigVersion
               , "Folders"  .= getFolderConfigs
               , "Devices"  .= getDeviceConfigs
               , "GUI"      .= getGuiConfig
               , "Options"  .= getOptionsConfig
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

instance ToJSON FolderConfig where
    toJSON FolderConfig {..} = 
        object [ "ID"              .= getId             
               , "Path"            .= getPath           
               , "Devices"         .= map FolderDeviceConfig getFolderDevices
               , "ReadOnly"        .= getReadOnly       
               , "RescanIntervalS" .= getRescanIntervalS
               , "IgnorePerms"     .= getIgnorePerms    
               , "Versioning"      .= getVersioning     
               , "LenientMtimes"   .= getLenientMtimes  
               , "Copiers"         .= getCopiers        
               , "Pullers"         .= getPullers        
               , "Finishers"       .= getFinishers      
               , "Invalid"         .= getFolderInvalid  
               ]


-------------------------------------------------------------------------------
-- VERSIONING CONFIG -----
-------------------------------------------------------------------------------

-- | Information about versioning.
data VersioningConfig = VersioningConfig {
      getType   :: Text
    , getParams :: M.Map Text Text
    } deriving Show

instance FromJSON VersioningConfig where
    parseJSON (Object v) =
        VersioningConfig <$> (v .: "Type")
                         <*> (v .: "Params")
    parseJSON _          = mzero

instance ToJSON VersioningConfig where
    toJSON VersioningConfig {..} =
        object [ "Type"   .= getType
               , "Params" .= getParams
               ]


-------------------------------------------------------------------------------
-- DEVICE CONFIG -----
-------------------------------------------------------------------------------

-- | Device specific configuration information.
data DeviceConfig = DeviceConfig {
      getDeviceId    :: DeviceId
    , getDeviceName  :: Text
    , getAddresses   :: [AddressType]
    , getCompression :: Bool
    , getCertName    :: Text
    , getIntroducer  :: Bool
    } deriving (Show)

instance FromJSON DeviceConfig where
    parseJSON (Object v) =
        DeviceConfig <$> (v .: "DeviceID")
                     <*> (v .: "Name")
                     <*> (map decodeAddressType <$> (v .: "Addresses"))
                     <*> (v .: "Compression")
                     <*> (v .: "CertName")
                     <*> (v .: "Introducer")
    parseJSON _          = mzero

instance ToJSON DeviceConfig where
    toJSON DeviceConfig {..} =
        object [ "DeviceID"     .= getDeviceId
               , "Name"         .= getDeviceName
               , "Addresses"    .= map encodeAddressType getAddresses 
               , "Compression"  .= getCompression 
               , "CertName"     .= getCertName 
               , "Introducer"   .= getIntroducer 
               ]


-------------------------------------------------------------------------------
-- FOLDER-DEVICE CONFIG -----
-------------------------------------------------------------------------------

data FolderDeviceConfig = FolderDeviceConfig {
      getFolderDeviceId :: DeviceId
    } deriving (Show)

instance FromJSON FolderDeviceConfig where
    parseJSON (Object v) = FolderDeviceConfig <$> (v .: "DeviceID")
    parseJSON _          = mzero

instance ToJSON FolderDeviceConfig where
    toJSON (FolderDeviceConfig deviceId) = 
        object [ "DeviceID" .= deviceId ]


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
    } deriving (Show)

instance FromJSON GuiConfig where
    parseJSON (Object v) =
        GuiConfig <$> (v .: "Enabled")
                  <*> (decodeApiKey <$> (v .: "APIKey"))
                  <*> (parseAddr <$> (v .: "Address"))
                  <*> (v .: "User")
                  <*> (v .: "Password")
                  <*> (v .: "UseTLS")
    parseJSON _          = mzero

instance ToJSON GuiConfig where
    toJSON GuiConfig {..} =
        object [ "Enabled"  .= getEnabled
               , "APIKey"   .= encodeApiKey getApiKey
               , "Address"  .= encodeAddr getGuiAddress 
               , "User"     .= getUser 
               , "Password" .= getPassword 
               , "UseTLS"   .= getUseTLS 
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

instance ToJSON OptionsConfig where
    toJSON OptionsConfig {..} =
        object [ "ListenAddress"           .= map encodeAddr getListenAddress          
               , "GlobalAnnServers"        .= getGlobalAnnServers          
               , "GlobalAnnEnabled"        .= getGlobalAnnEnabled          
               , "LocalAnnEnabled"         .= getLocalAnnEnabled          
               , "LocalAnnPort"            .= getLocalAnnPort           
               , "LocalAnnMCAddr"          .= getLocalAnnMCAddr          
               , "MaxSendKbps"             .= getMaxSendKbps            
               , "MaxRecvKbps"             .= getMaxRecvKbps            
               , "ReconnectIntervalS"      .= getReconnectIntervalS          
               , "StartBrowser"            .= getStartBrowser           
               , "UPnPEnabled"             .= getUPnPEnabled            
               , "UPnPLease"               .= getUPnPLease              
               , "UPnPRenewal"             .= getUPnPRenewal            
               , "URAccepted"              .= getURAccepted             
               , "URUniqueID"              .= getURUniqueID             
               , "RestartOnWakeup"         .= getRestartOnWakeup          
               , "AutoUpgradeIntervalH"    .= getAutoUpgradeIntervalH          
               , "KeepTemporariesH"        .= getKeepTemporariesH          
               , "CacheIgnoredFiles"       .= getCacheIgnoredFiles          
               , "ProgressUpdateIntervalS" .= getProgressUpdateIntervalS          
               , "SymlinksEnabled"         .= getSymlinksEnabled          
               ]

