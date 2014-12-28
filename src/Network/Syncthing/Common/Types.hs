
{-# LANGUAGE OverloadedStrings #-}

module Network.Syncthing.Common.Types
    ( DeviceId
    , FolderName
    , SubPath
    , Addr
    , defaultFolder
    ) where

import           Data.Text (Text)


type DeviceId   = Text

type FolderName = Text

type SubPath    = Text

type Addr       = Text

defaultFolder :: FolderName
defaultFolder = "default"

