
{-# LANGUAGE OverloadedStrings #-}

module Network.Pulse.Post.Ping 
    ( Ping(..)
    , ping
    ) where
    
import Network.Pulse.Common.Ping
import Network.Pulse.Internal.Query 
import Network.Pulse.Types
import Data.Aeson

ping :: Pulse Ping
ping = query $ PulseRequest { path = "/rest/ping", method = Post payload, params = [] }
    where payload = toJSON ()

