
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Network.Pulse.Post.Ping 
    ( Ping(..)
    , ping
    ) where
    
import Network.Pulse.Common.Ping
import Network.Pulse.Query 
import Network.Pulse.Types
import Data.Aeson

ping :: (MonadPulse (PulseM m), Monad m) => PulseM m Ping
ping = query $ 
    PulseRequest { 
          path   = "/rest/ping"
        , method = Post payload
        , params = []
    }
    where payload = toJSON ()

