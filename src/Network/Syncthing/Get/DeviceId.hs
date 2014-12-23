
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Syncthing.Get.DeviceId
    ( deviceId
    ) where

import           Control.Applicative            ((<$>))
import           Control.Monad                  (MonadPlus (mzero), (>=>))
import           Data.Aeson                     (FromJSON, Value (..), parseJSON, (.:))
import           Data.HashMap.Lazy              (member)
import           Data.Text                      ()

import           Network.Syncthing.Common.Types
import           Network.Syncthing.Query
import           Network.Syncthing.Types
import           Network.Syncthing.Utils        (decodeDeviceError)


deviceId :: MonadSync m => DeviceId -> SyncM m DeviceId
deviceId = deviceId' >=> either (liftLeft . InvalidDeviceId) liftRight

deviceId' :: MonadSync m => DeviceId -> SyncM m (Either DeviceError DeviceId)
deviceId' device = query $ getRequest { path   = "/rest/deviceid"
                                      , params = [("id", device)]
                                      }

instance FromJSON (Either DeviceError DeviceId) where
    parseJSON (Object v) = result
      where hasId        = member "id" v
            result       = parseIdResult hasId v
    parseJSON _          = mzero

parseIdResult hasId v
    | hasId     = Right <$> v .: "id"
    | otherwise = Left  <$> (decodeDeviceError <$> v .: "error")

