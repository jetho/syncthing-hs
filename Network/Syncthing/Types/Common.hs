
{-# LANGUAGE OverloadedStrings #-}

module Network.Syncthing.Types.Common
    (
      DeviceId
    , FolderName
    , Path
    , Host
    , Port
    , Addr
    , defaultFolder
    ) where

import           Data.Text (Text)


type DeviceId   = Text

type FolderName = Text

type Path       = Text

type Host       = Text

type Port       = Int

type Addr       = (Host, Maybe Port)

-- | The default folder name.
defaultFolder :: FolderName
defaultFolder = "default"

