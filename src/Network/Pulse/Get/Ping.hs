
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Network.Pulse.Get.Ping 
    ( Ping(..)
    , ping
    ) where
    
import Network.Pulse.Common.Ping
import Network.Pulse.Query 
import Network.Pulse.Types


ping :: (MonadPulse (PulseM m), Monad m) => PulseM m Ping
ping = query $ 
    PulseRequest { 
          path   = "/rest/ping"
        , method = Get
        , params = []
    }

