
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module SyncthingTest.JSONInstances where

import           Data.Aeson

import           Network.Syncthing


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
