
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Network.Pulse.Get.Version
    ( Version(..)
    , version
    ) where
    
import Control.Applicative          ((<$>), (<*>))
import Control.Monad                (MonadPlus (mzero))
import Data.Aeson                   
import Data.Text
import Network.Pulse.Query 
import Network.Pulse.Types


data Version = Version { 
      getArch        :: Text
    , getLongVersion :: Text
    , getOs          :: Text
    , getVersion     :: Text
    } deriving (Show)

version :: (MonadPulse m) => PulseM m Version
version = query $ 
    PulseRequest { 
          path   = "/rest/version"
        , method = Get
        , params = []
    }

instance FromJSON Version where
    parseJSON (Object v) = 
        Version <$> (v .: "arch")
                <*> (v .: "longVersion")
                <*> (v .: "os")
                <*> (v .: "version")
    parseJSON _          = mzero

