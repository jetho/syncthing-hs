
{-# LANGUAGE OverloadedStrings #-}

module Network.Syncthing.Types.Common
    (
      Device
    , FolderName
    , Path
    , Host
    , Port
    , Addr
    , defaultFolder
    ) where

import           Data.Text (Text)


type Device     = Text

type FolderName = Text

type Path       = Text

type Host       = Text

type Port       = Int

type Addr       = (Host, Maybe Port)

-- | The default folder name.
defaultFolder :: FolderName
defaultFolder = "default"

