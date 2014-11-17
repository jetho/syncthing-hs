
{-# LANGUAGE OverloadedStrings #-}

module Network.Pulse 
    ( 
    -- * The Pulse Monad 
      Pulse
    , pulse
    , withManager
    -- * Configuration
    , PulseConfig
    , pServer
    , pApiKey
    , pAuth
    , pManager
    , defaultPulseConfig
    -- * Error Handling
    , PulseError(..)
    ) where

import qualified Network.Wreq       as W
import Network.HTTP.Client.TLS      (tlsManagerSettings)
import Control.Monad.Trans.Either   (runEitherT)
import Control.Monad.Trans.Reader   (runReaderT)
import Data.Aeson                   (FromJSON)
import Control.Lens                 (Lens', (&), (^.), (.~))
import Data.Text                    (Text)
import Network.HTTP.Client          (Manager, ManagerSettings)

import Network.Pulse.Types
import qualified Network.Pulse.Lens as PL


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

-- | A lens for configuring the server address. Use the ADDRESS:PORT format.
--
-- Example:
--
-- @
--let cfg = 'defaultPulseConfig' 'Control.Lens.&' 'pApiKey' 'Control.Lens..~' \"192.168.0.10:8080\"
--'pulse' cfg 'Network.Pulse.Get.ping' @
pServer :: Lens' PulseConfig Text
pServer  = PL.pServer

-- | A lens for specifying the Pulse API Key.
--
-- Example:
--
-- @
--let cfg = 'defaultPulseConfig' 'Control.Lens.&' 'pApiKey' 'Control.Lens.?~' \"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\"
--'pulse' cfg 'Network.Pulse.Get.ping' @
pApiKey :: Lens' PulseConfig (Maybe Text)
pApiKey  = PL.pApiKey

pAuth :: Lens' PulseConfig (Maybe W.Auth)
pAuth    = PL.pAuth

pManager :: Lens' PulseConfig (Either ManagerSettings Manager)
pManager = PL.pManager

