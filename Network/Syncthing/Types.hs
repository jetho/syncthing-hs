

-- |
-- Module      : Network.Syncthing.Types
-- Copyright   : (c) 2014 Jens Thomas
--
-- License     : BSD-style
-- Maintainer  : jetho@gmx.de
-- Stability   : experimental
-- Portability : GHC
--
-- Type synonyms and data types.

module Network.Syncthing.Types
    (  
    -- * Type Synonyms
      Server
    , Device
    , FolderName
    , Path
    , Host
    , Port
    , Addr
 
    -- * Data Types
    , CacheEntry(..)
    , Config(..)
    , AddressType(..)
    , FolderConfig(..)
    , DeviceConfig(..)
    , VersioningConfig(..)
    , GuiConfig(..)
    , OptionsConfig(..)
    , Connections(..)
    , Connection(..)
    , DeviceInfo(..)
    , DirTree(..)
    , Error(..)
    , Ignore(..)
    , Model(..)
    , ModelState(..)
    , Need(..)
    , DBFile(..)
    , FileInfo(..)
    , FolderInfo(..)
    , LastFile(..)
    , System(..)
    , SystemMsg(..)
    , Upgrade(..)
    , UsageReport(..)
    , Version(..)   
    ) where

import Network.Syncthing.Internal.Types

