
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
    ( module Network.Syncthing.Get.Ping
    , module Network.Syncthing.Get.Sync
    , module Network.Syncthing.Get.Version
    ) where

import           Network.Syncthing.Get.Ping
import           Network.Syncthing.Get.Sync
import           Network.Syncthing.Get.Version
