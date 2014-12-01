
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Pulse.Get.Sync
    ( sync
    ) where

import           Control.Applicative ((<$>))
import           Control.Monad       (MonadPlus (mzero))
import           Data.Aeson
import           Network.Pulse.Query
import           Network.Pulse.Types


data Sync = Sync {
      getConfigInSync :: Bool
    } deriving (Show)

sync :: MonadPulse m => PulseM m Bool
sync = getConfigInSync <$> sync'
    where
        sync' :: MonadPulse m => PulseM m Sync
        sync' = query $
            PulseRequest {
                  path   = "/rest/config/sync"
                , method = Get
                , params = []
            }

instance FromJSON Sync where
    parseJSON (Object v) = Sync <$> (v .: "configInSync")
    parseJSON _          = mzero
