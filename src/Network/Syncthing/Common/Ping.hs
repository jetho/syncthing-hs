
{-# LANGUAGE OverloadedStrings #-}

module Network.Syncthing.Common.Ping
    ( Ping(..)
    ) where

import           Control.Applicative ((<$>))
import           Control.Monad       (MonadPlus (mzero))
import           Data.Aeson          (FromJSON, Value (..), parseJSON, (.:))
import           Data.Text           (Text)

data Ping = Ping {
      getPing :: Text
    } deriving (Show)

instance FromJSON Ping where
    parseJSON (Object v) = Ping <$> (v .: "ping")
    parseJSON _          = mzero

