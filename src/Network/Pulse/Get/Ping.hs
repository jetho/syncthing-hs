
{-# LANGUAGE OverloadedStrings #-}

module Network.Pulse.Get.Ping 
    ( Ping(..)
    , ping
    ) where
    
import Control.Applicative          ((<$>), (<*>))
import Control.Monad                (MonadPlus (mzero))
import Data.Aeson                   
import Network.Pulse.Internal.Query 
import Network.Pulse.Types

data Ping = Ping { getPing   :: String } deriving (Show)

ping :: Pulse Ping
ping = query $ PulseRequest { path = "/rest/ping", method = Get, params = [] }

instance FromJSON Ping where
    parseJSON (Object v) = Ping <$> (v .: "ping")
    parseJSON _          = mzero

