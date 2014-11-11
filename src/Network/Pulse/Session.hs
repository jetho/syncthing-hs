
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.Pulse.Session 
    ( PulseSession(..)
    , newPulseSession
    , closePulseSession
    , withPulseSession'
    , defaultPulseSession
    , pServer
    , pApiKey
    , pOptions
    , pManager
    ) where

import Network.HTTP.Client     (newManager, closeManager, Manager, ManagerSettings)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Wreq            (withManager, defaults, manager)
import Network.Wreq.Types      (Options)
import Data.Aeson              (FromJSON)
import Control.Lens            (makeLenses, Lens', (.~), (&), (^.))
import Data.Text               as T


data PulseSession = PulseSession { 
      _pServer    :: T.Text
    , _pApiKey    :: Maybe T.Text
    , _pOptions   :: Options
    }

$(makeLenses ''PulseSession)

pManager :: Lens' PulseSession (Either ManagerSettings Manager)
pManager = pOptions . manager

newPulseSession :: IO PulseSession
newPulseSession = do
    mgr <- newManager tlsManagerSettings
    return $ defaultPulseSession & pManager .~ Right mgr

closePulseSession :: PulseSession -> IO ()
closePulseSession session = either doNothing closeManager mgr
    where
        doNothing   = const $ return ()
        mgr         = session ^. pManager

withPulseSession' :: (PulseSession -> IO a) -> IO a
withPulseSession' act = 
    withManager $ \opts -> 
        act $ defaultPulseSession & pOptions .~ opts 
    
defaultPulseSession :: PulseSession
defaultPulseSession = PulseSession { 
      _pServer   = "127.0.0.1:8080"
    , _pApiKey   = Nothing
    , _pOptions  = defaults
    }

