
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE FlexibleInstances #-}


module SyncthingTest.JSONInstances where

import           Control.Applicative              ((<$>), pure)
import           Data.Aeson
import           Data.Maybe                       (fromMaybe)
import qualified Data.Text                        as T

import           Network.Syncthing
import           Network.Syncthing.Internal.Utils


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
    toJSON Version {..} =
        object [ "arch"         .= getArch
               , "longVersion"  .= getLongVersion
               , "os"           .= getOs
               , "version"      .= getVersion
               ]

instance ToJSON Ping where
    toJSON Ping {..} = object [ "ping" .= getPing ]

instance ToJSON Completion where
    toJSON Completion {..} = object [ "completion" .= getCompletion ]

instance ToJSON Sync where
    toJSON Sync {..} = object [ "configInSync" .= getSync ]

instance ToJSON CacheEntry where
    toJSON CacheEntry {..} =
        object [ "Address"  .= encodeAddr getAddr
               , "Seen"     .= encodeUTC getSeen
               ]

instance ToJSON Connection where
    toJSON Connection {..} =
        object [ "At"            .= encodeUTC getAt
               , "InBytesTotal"  .= getInBytesTotal
               , "OutBytesTotal" .= getOutBytesTotal
               , "Address"       .= encodeAddr getAddress
               , "ClientVersion" .= getClientVersion
               ]

instance ToJSON Model where
    toJSON Model {..} =
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
    toJSON Upgrade {..} =
        object [ "latest"  .= getLatest
               , "newer"   .= getNewer
               , "running" .= getRunning
               ]

instance ToJSON Ignore where
    toJSON Ignore {..} =
        object [ "ignore"   .= getIgnores
               , "patterns" .= getPatterns
               ]

instance ToJSON Need where
    toJSON Need {..} =
        object [ "progress" .= getProgress
               , "queued"   .= getQueued
               , "rest"     .= getRest
               ]

instance ToJSON Progress where
    toJSON Progress {..} =
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

