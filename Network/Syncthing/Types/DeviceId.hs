
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Syncthing.Types.DeviceId where

import           Control.Applicative              ((<$>))
import           Control.Monad                    (MonadPlus (mzero))
import           Data.Aeson                       (FromJSON, Value (..), parseJSON, (.:))
import           Data.HashMap.Lazy                (member)
import           Data.Text                        ()

import           Network.Syncthing.Internal.Error
import           Network.Syncthing.Types.Common


instance FromJSON (Either DeviceError Device) where
    parseJSON (Object v) = result
      where hasId        = member "id" v
            result       = parseIdResult hasId v
    parseJSON _          = mzero

parseIdResult hasId v
    | hasId     = Right <$> v .: "id"
    | otherwise = Left  <$> (decodeDeviceError <$> v .: "error")

