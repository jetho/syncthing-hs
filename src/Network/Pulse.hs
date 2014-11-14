
{-# LANGUAGE OverloadedStrings #-}

module Network.Pulse 
    ( PulseConfig
    , withManager
    , defaultPulseConfig
    , Pulse
    , pulse
    , PulseError(..)
    , pServer
    , pApiKey
    , pAuth
    , pManager
    ) where

import qualified Network.Wreq     as W
import Network.HTTP.Client.TLS    (tlsManagerSettings)
import Control.Monad.Trans.Either (runEitherT)
import Control.Monad.Trans.Reader (runReaderT)
import Data.Aeson                 (FromJSON)
import Control.Lens               ((&), (^.), (.~))

import Network.Pulse.Types


withManager :: FromJSON a => (PulseConfig -> IO (Either PulseError a)) -> IO (Either PulseError a)
withManager act = 
    W.withManager $ \opts -> 
        act $ defaultPulseConfig & pManager .~ (opts ^. W.manager)
    
pulse :: FromJSON a => PulseConfig -> Pulse a -> IO (Either PulseError a)
pulse config action = flip runReaderT config $ runEitherT action

defaultPulseConfig :: PulseConfig
defaultPulseConfig = PulseConfig { 
      _pServer   = "127.0.0.1:8080"
    , _pApiKey   = Nothing
    , _pAuth     = Nothing
    , _pManager  = Left tlsManagerSettings 
    }

