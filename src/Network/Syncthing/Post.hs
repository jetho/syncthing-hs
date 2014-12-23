
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      : Network.Syncthing.Post
-- Copyright   : (c) 2014 Jens Thomas
--
-- License     : BSD-style
-- Maintainer  : jetho@gmx.de
-- Stability   : experimental
-- Portability : GHC
--
-- The POST requests.

module Network.Syncthing.Post
    ( 
    -- * Common Types
      Types.DeviceId
    , Types.Folder
    , Types.SubPath
    , Types.Addr
 
    -- * Defaults
    , Types.defaultFolder

    -- * Request functions
    , module Network.Syncthing.Post.Ping
    , module Network.Syncthing.Post.Discovery
    , module Network.Syncthing.Post.Error
    , module Network.Syncthing.Post.Ignores
    , module Network.Syncthing.Post.Scan
    , module Network.Syncthing.Post.System
    ) where

import qualified Network.Syncthing.Common.Types   as Types

import           Network.Syncthing.Post.Ping
import           Network.Syncthing.Post.Discovery
import           Network.Syncthing.Post.Error
import           Network.Syncthing.Post.Ignores
import           Network.Syncthing.Post.Scan
import           Network.Syncthing.Post.System

