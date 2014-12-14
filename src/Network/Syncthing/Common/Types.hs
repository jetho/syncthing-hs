
{-# LANGUAGE OverloadedStrings #-}

module Network.Syncthing.Common.Types
    ( DeviceId
    , Folder
    , SubPath
    , defaultFolder
    ) where

import           Data.Text (Text)


type DeviceId = Text

type Folder   = Text

type SubPath  = Text

defaultFolder :: Folder
defaultFolder = "default"

