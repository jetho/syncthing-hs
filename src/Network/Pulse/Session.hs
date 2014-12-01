
{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Network.Pulse.Session
-- Copyright   : (c) 2014 Jens Thomas
--
-- License     : BSD-style
-- Maintainer  : jetho@gmx.de
-- Stability   : experimental
-- Portability : GHC
--
-- This module provides functions for manual session handling.

module Network.Pulse.Session
    ( PulseSession
    , newPulseSession
    , closePulseSession
    , runSession
    ) where

import           Control.Lens        ((&), (.~), (^.))
import           Data.Aeson          (FromJSON)
import           Network.HTTP.Client (closeManager, newManager)

import           Network.Pulse
import           Network.Pulse.Types

-- | Holds the session configuration and the connection manager.
newtype PulseSession = PulseSession { getConfig :: PulseConfig }

-- | Creates a new Pulse session for the provided configuration. You should
-- reuse the session whenever possible because of connection sharing.
newPulseSession :: PulseConfig -> IO PulseSession
newPulseSession config = do
    mgr <- createManager $ config ^. pManager
    return . PulseSession $ config & pManager .~ Right mgr
    where
        createManager (Left settings)   = newManager settings
        createManager (Right mgr)       = return mgr

-- | Closes a Pulse session.
closePulseSession :: PulseSession -> IO ()
closePulseSession session = either doNothing closeManager mgr
    where
        doNothing   = const $ return ()
        mgr         = getConfig session ^. pManager

-- | Runs a Pulse request using connection sharing within a session.
runSession :: FromJSON a => PulseSession -> PulseM IO a -> IO (Either PulseError a)
runSession = pulse . getConfig

