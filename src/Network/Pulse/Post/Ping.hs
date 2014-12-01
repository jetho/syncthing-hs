{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Pulse.Post.Ping
    ( Ping(..)
    , ping
    ) where

import           Data.Aeson
import           Network.Pulse.Common.Ping
import           Network.Pulse.Query
import           Network.Pulse.Types

ping :: (MonadPulse m) => PulseM m Ping
