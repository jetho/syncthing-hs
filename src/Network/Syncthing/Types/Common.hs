
{-# LANGUAGE OverloadedStrings #-}

module Network.Syncthing.Types.Common
    (
    -- * Type Synonyms
      DeviceId
    , FolderName
    , Path
    , Host
    , Port
    , Addr

    -- * Defaults
    , defaultFolder
    ) where

import           Data.Text (Text)


type DeviceId   = Text

type FolderName = Text

type Path       = Text

type Host       = Text

type Port       = Int

type Addr       = (Host, Maybe Port)

defaultFolder :: FolderName
defaultFolder = "default"

