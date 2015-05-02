
{-# LANGUAGE OverloadedStrings #-}

module Network.Syncthing.Types.DeviceInfo
    ( DeviceInfo(..)
    ) where

import           Control.Applicative              ((<$>))
import           Control.Monad                    (MonadPlus (mzero))
import           Data.Aeson                       (FromJSON, Value (..), parseJSON, (.:))
import           Data.Time.Clock                  (UTCTime)

import           Network.Syncthing.Internal.Utils (toUTC)


-- | Contains information about a device.
data DeviceInfo = DeviceInfo    {
      getLastSeen   :: Maybe UTCTime
    } deriving (Eq, Show)

instance FromJSON DeviceInfo where
    parseJSON (Object v) = DeviceInfo <$> (toUTC <$> v .: "lastSeen")
    parseJSON _          = mzero

