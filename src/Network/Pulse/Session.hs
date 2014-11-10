{-# LANGUAGE OverloadedStrings #-}

module Network.Pulse.Session 
    ( PulseSession(..)
    , newPulseSession
    , closePulseSession
    , withPulseSession
    , defaultPulseSession
    ) where

import Network.Pulse.Types     (PulseError)
import Network.HTTP.Client     (newManager, closeManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Wreq            (withManager, defaults, manager)
import Network.Wreq.Types      (Options)
import Data.Aeson              (FromJSON)
import Control.Lens            ((.~), (&), (^.))
import Data.Text               as T


data PulseSession = PulseSession { 
      pServer    :: T.Text
    , pApiKey    :: Maybe T.Text
    , pOptions   :: Options
    }

newPulseSession :: IO PulseSession
newPulseSession = do
    mgr <- newManager tlsManagerSettings
    let opts = defaults & manager .~ Right mgr 
    return $ defaultPulseSession { pOptions = opts }

closePulseSession :: PulseSession -> IO ()
closePulseSession session = either doNothing closeManager mgr
    where
        doNothing   = const $ return ()
        mgr         = pOptions session ^. manager

withPulseSession :: FromJSON a => (PulseSession -> IO (Either PulseError a)) -> IO (Either PulseError a)
withPulseSession act = 
    withManager $ \opts -> 
        act $ defaultPulseSession { pOptions = opts }
    
defaultPulseSession :: PulseSession
defaultPulseSession = PulseSession { 
      pServer   = "127.0.0.1:8080"
    , pApiKey   = Nothing
    , pOptions  = defaults
    }

