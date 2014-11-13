
{-# LANGUAGE OverloadedStrings #-}

module Network.Pulse.Session 
    ( PulseSession
    , newPulseSession
    , closePulseSession
    , runSession
    ) where

import Network.HTTP.Client     (newManager, closeManager)
import Control.Lens            ((.~), (&), (^.))
import Data.Aeson              (FromJSON)

import Network.Pulse.Types     
import Network.Pulse           


newtype PulseSession = PulseSession { getConfig :: PulseConfig }

newPulseSession :: PulseConfig -> IO PulseSession
newPulseSession config = do
    mgr <- createManager $ config ^. pManager
    return . PulseSession $ config & pManager .~ Right mgr
    where
        createManager (Left settings)   = newManager settings
        createManager (Right mgr)       = return mgr

closePulseSession :: PulseSession -> IO ()
closePulseSession session = either doNothing closeManager mgr
    where
        doNothing   = const $ return ()
        mgr         = getConfig session ^. pManager

runSession :: FromJSON a => PulseSession -> Pulse a -> IO (Either PulseError a)
runSession = pulse . getConfig

