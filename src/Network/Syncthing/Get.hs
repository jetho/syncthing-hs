
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
    ( module Ping
    , module Sync
    , module Upgrade
    , module Version
    ) where

import           Network.Syncthing.Get.Ping    as Ping
import           Network.Syncthing.Get.Sync    as Sync
import           Network.Syncthing.Get.Upgrade as Upgrade
import           Network.Syncthing.Get.Version as Version
