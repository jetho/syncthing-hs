
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
import Control.Monad.Trans.Either (EitherT)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.IO.Class     (MonadIO)
import Data.Aeson                 (FromJSON)
import Control.Lens               ((&), (^.), (.~))

import Network.Pulse.Types


type Pulse a = EitherT PulseError (ReaderT PulseConfig IO) a

withManager :: FromJSON a => (PulseConfig -> IO (Either PulseError a)) -> IO (Either PulseError a)
withManager act = 
    W.withManager $ \opts -> 
        act $ defaultPulseConfig & pManager .~ (opts ^. W.manager)
    
query :: FromJSON a => PulseRequest -> Pulse a
query = undefined

pulse :: FromJSON a => PulseConfig -> Pulse a -> IO (Either PulseError a)
pulse = undefined

defaultPulseConfig :: PulseConfig
defaultPulseConfig = PulseConfig { 
      _pServer   = "127.0.0.1:8080"
    , _pApiKey   = Nothing
    , _pAuth     = Nothing
    , _pManager  = Left tlsManagerSettings 
    }

