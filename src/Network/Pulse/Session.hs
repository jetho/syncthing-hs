
{-# LANGUAGE OverloadedStrings #-}

module Network.Pulse.Session 
    ( PulseSession
    , newPulseSession
    , closePulseSession
    ) where

import Network.HTTP.Client     (newManager, closeManager)
import Control.Lens            ((.~), (&), (^.))
import Network.Pulse.Types


newtype PulseSession = PulseSession { getConfig :: PulseConfig }

newPulseSession :: PulseConfig -> IO PulseSession
newPulseSession config = do
    mgr <- createManager $ config ^. pManager
    return . PulseSession $ config & pManager .~ Right mgr
    where
        createManager (Left settings)   = newManager settings
        createManager (Right manager)   = return manager

closePulseSession :: PulseSession -> IO ()
closePulseSession session = either doNothing closeManager mgr
    where
        doNothing   = const $ return ()
        mgr         = getConfig session ^. pManager

