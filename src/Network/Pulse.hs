
{-# LANGUAGE OverloadedStrings #-}

module Network.Pulse 
    ( PulseSession(..)
    , withPulseSession
    , newPulseSession
    , closePulseSession
    , defaultPulseSession
    , Pulse
    , pulse
    , PulseError(..)
    , pServer
    , pApiKey
    , pOptions
    , pManager
    ) where

import Network.Wreq
import Control.Monad.Trans.Either (EitherT)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.IO.Class     (MonadIO)
import Data.Aeson                 (FromJSON)

import Network.Pulse.Session
import Network.Pulse.Types


type Pulse a = EitherT PulseError (ReaderT PulseSession IO) a

withPulseSession :: FromJSON a => (PulseSession -> IO (Either PulseError a)) -> IO (Either PulseError a)
withPulseSession = withPulseSession'

query :: FromJSON a => PulseRequest -> Pulse a
query = undefined

pulse :: (MonadIO m, FromJSON a) => PulseSession -> Pulse a -> m (Either PulseError a)
pulse = undefined

