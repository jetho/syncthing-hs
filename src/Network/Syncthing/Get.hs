
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      : Network.Syncthing.Get
-- Copyright   : (c) 2014 Jens Thomas
--
-- License     : BSD-style
-- Maintainer  : jetho@gmx.de
-- Stability   : experimental
-- Portability : GHC
--
-- The GET requests.

module Network.Syncthing.Get
    (
    -- * Common Types
      Types.DeviceId
    , Types.Folder

    -- * Request functions
    , module Network.Syncthing.Get.Completion
    , module Network.Syncthing.Get.Ping
    , module Network.Syncthing.Get.Sync
    , module Network.Syncthing.Get.System
    , module Network.Syncthing.Get.Upgrade
    , module Network.Syncthing.Get.Version
    ) where

import qualified Network.Syncthing.Common.Types   as Types

import           Network.Syncthing.Get.Completion
import           Network.Syncthing.Get.Ping
import           Network.Syncthing.Get.Sync
import           Network.Syncthing.Get.System
import           Network.Syncthing.Get.Upgrade
import           Network.Syncthing.Get.Version

