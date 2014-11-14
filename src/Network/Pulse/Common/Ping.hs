
{-# LANGUAGE OverloadedStrings #-}

module Network.Pulse.Common.Ping 
    ( Ping(..)
    ) where
    
import Control.Applicative          ((<$>), (<*>))
import Control.Monad                (MonadPlus (mzero))
import Data.Aeson                   

data Ping = Ping { getPing   :: String } deriving (Show)

instance FromJSON Ping where
    parseJSON (Object v) = Ping <$> (v .: "ping")
    parseJSON _          = mzero

