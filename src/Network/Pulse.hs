{-# LANGUAGE OverloadedStrings #-}

module Network.Pulse 
    ( PulseSession(..)
    , withPulseSession
    , newPulseSession
    , closePulseSession
    , defaultPulseSession
    , Pulse
    , query
    , pulse
    , PulseError(..)
    ) where

import Network.Wreq
import Control.Monad.Trans.Either 
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Data.Aeson
import Data.Text
import Control.Lens

import Network.Pulse.Session
import Network.Pulse.Types


type Pulse a = EitherT PulseError (ReaderT PulseSession IO) a



query :: FromJSON a => PulseRequest -> Pulse a
query = undefined


pulse :: (MonadIO m, FromJSON a) => PulseSession -> Pulse a -> m (Either PulseError a)
pulse = undefined



pulseServer :: Lens' PulseSession Text
pulseServer = undefined

