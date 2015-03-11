
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE FlexibleInstances #-}


module Properties.JsonInstances where

import           Control.Applicative              ((<$>), pure)
import           Data.Aeson                       hiding (Error)
import           Data.Maybe                       (fromMaybe)
import qualified Data.Text                        as T

import           Network.Syncthing.Internal.Error
import           Network.Syncthing.Internal.Utils
import           Network.Syncthing.Types


singleField :: ToJSON a => T.Text -> a -> Value
singleField fieldName = object . pure . (fieldName .=)

encodeMaybe = fromMaybe ""

encodeUTC = encodeMaybe . fmap fromUTC 

encodeInvalid = encodeMaybe . fmap T.unpack 

encodeModelState = encodeMaybe . fmap encodeState
  where encodeState state = 
            case state of
                Idle     -> "idle"
                Scanning -> "scanning"
                Cleaning -> "cleaning"
                Syncing  -> "syncing"

instance ToJSON Version where
    toJSON Version{..} =
        object [ "arch"         .= getArch
               , "longVersion"  .= getLongVersion
               , "os"           .= getOs
               , "version"      .= getVersion
               ]

instance ToJSON Ping where
    toJSON = singleField "ping" . getPing

instance ToJSON Completion where
    toJSON = singleField "completion" . getCompletion 

instance ToJSON Sync where
    toJSON = singleField "configInSync" . getSync 

instance ToJSON CacheEntry where
    toJSON CacheEntry{..} =
        object [ "Address"  .= encodeAddr getAddr
               , "Seen"     .= encodeUTC getSeen
               ]

instance ToJSON Connection where
    toJSON Connection{..} =
        object [ "At"            .= encodeUTC getAt
               , "InBytesTotal"  .= getInBytesTotal
               , "OutBytesTotal" .= getOutBytesTotal
               , "Address"       .= encodeAddr getAddress
               , "ClientVersion" .= getClientVersion
               ]

instance ToJSON Model where
    toJSON Model{..} =
        object [ "globalBytes"   .= getGlobalBytes   
               , "globalDeleted" .= getGlobalDeleted 
               , "globalFiles"   .= getGlobalFiles   
               , "inSyncBytes"   .= getInSyncBytes   
               , "inSyncFiles"   .= getInSyncFiles   
               , "localBytes"    .= getLocalBytes    
               , "localDeleted"  .= getLocalDeleted  
               , "localFiles"    .= getLocalFiles    
               , "needBytes"     .= getNeedBytes     
               , "needFiles"     .= getNeedFiles     
               , "state"         .= encodeModelState getState         
               , "stateChanged"  .= encodeUTC getStateChanged  
               , "invalid"       .= encodeInvalid getInvalid
               , "version"       .= getModelVersion  
               ]

instance ToJSON Upgrade where
    toJSON Upgrade{..} =
        object [ "latest"  .= getLatest
               , "newer"   .= getNewer
               , "running" .= getRunning
               ]

instance ToJSON Ignore where
    toJSON Ignore{..} =
        object [ "ignore"   .= getIgnores
               , "patterns" .= getPatterns
               ]

instance ToJSON Need where
    toJSON Need{..} =
        object [ "progress" .= getProgress
               , "queued"   .= getQueued
               , "rest"     .= getRest
               ]

instance ToJSON Progress where
    toJSON Progress{..} =
        object [ "Name"         .= getName            
               , "Flags"        .= getFlags           
               , "Modified"     .= getModified        
               , "Version"      .= getProgressVersion 
               , "LocalVersion" .= getLocalVersion    
               , "NumBlocks"    .= getNumBlocks       
               , "Size"         .= getSize            
               ]

instance ToJSON (Either DeviceError Device) where
    toJSON = object . pure . either deviceError deviceId 
      where deviceError = ("error" .=) . encodeDeviceError 
            deviceId    = ("id" .=) 

encodeDeviceError :: DeviceError -> T.Text
encodeDeviceError err = 
    case err of
        IncorrectLength     -> "device ID invalid: incorrect length"
        IncorrectCheckDigit -> "check digit incorrect"
        OtherDeviceError e  -> e

instance ToJSON SystemMsg where
    toJSON msg = object [ "ok" .= decodedSystemMsg ]
      where 
        decodedSystemMsg = case msg of
            Restarting       -> "restarting"
            ShuttingDown     -> "shutting down"
            ResettingFolders -> "resetting folders"
            OtherSystemMsg m -> m

instance ToJSON Error where
    toJSON Error{..} =
        object [ "Time"  .= encodeUTC getTime
               , "Error" .= getMsg
               ]

instance ToJSON Errors where
    toJSON = singleField "errors" . getErrors 

