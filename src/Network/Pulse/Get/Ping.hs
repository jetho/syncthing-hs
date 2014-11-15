
{-# LANGUAGE OverloadedStrings #-}

module Network.Pulse.Get.Ping 
    ( Ping(..)
    , ping
    ) where
    
import Network.Pulse.Common.Ping
import Network.Pulse.Query 
import Network.Pulse.Types

ping :: Pulse Ping
ping = query $ PulseRequest { path = "/rest/ping", method = Get, params = [] }

