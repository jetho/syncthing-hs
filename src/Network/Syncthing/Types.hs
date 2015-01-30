
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      : Network.Syncthing.Types
-- Copyright   : (c) 2014 Jens Thomas
--
-- License     : BSD-style
-- Maintainer  : jetho@gmx.de
-- Stability   : experimental
-- Portability : GHC
--
-- Common type definitions.

module Network.Syncthing.Types
    ( 
    -- * Type Synonyms
      DeviceId
    , FolderName
    , Path
    , Host
    , Port
    , Addr
    
    -- * Data Types
    , CacheEntry(..)
    , Connection(..)
    , Error(..)
    , Ignore(..)
    , Model(..)
    , ModelState(..)
    , Need(..)
    , Progress(..)
    , System(..)
    , SystemMsg(..)
    , Upgrade(..)
    , Version(..)
    ) where

import           Network.Syncthing.Types.CacheEntry
import           Network.Syncthing.Types.Common
import           Network.Syncthing.Types.Connection
import           Network.Syncthing.Types.Error
import           Network.Syncthing.Types.Ignore
import           Network.Syncthing.Types.Model
import           Network.Syncthing.Types.Need
import           Network.Syncthing.Types.System
import           Network.Syncthing.Types.SystemMsg
import           Network.Syncthing.Types.Upgrade
import           Network.Syncthing.Types.Version

