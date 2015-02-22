
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module SyncthingTest.JSONInstances where

import           Control.Applicative              ((<$>))
import           Data.Aeson
import           Data.Maybe                       (fromMaybe)
import qualified Data.Text                        as T

import           Network.Syncthing
import           Network.Syncthing.Internal.Utils


encodeUTC = fromMaybe "" . fmap fromUTC 

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

