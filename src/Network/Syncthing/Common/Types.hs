
{-# LANGUAGE OverloadedStrings #-}

module Network.Syncthing.Common.Types
    ( DeviceId
    , Folder
    , SubPath
    , Addr
    , defaultFolder
    ) where

import           Data.Text (Text)


type DeviceId = Text

type Folder   = Text

type SubPath  = Text

type Addr     = Text

defaultFolder :: Folder
defaultFolder = "default"

