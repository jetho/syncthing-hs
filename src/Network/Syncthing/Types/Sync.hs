
{-# LANGUAGE OverloadedStrings #-}

module Network.Syncthing.Types.Sync
    ( Sync(..)
    ) where

import           Control.Applicative              ((<$>))
import           Control.Monad                    (MonadPlus (mzero))
import           Data.Aeson                       (FromJSON, Value (..), parseJSON, (.:))


newtype Sync = Sync { getSync :: Bool } deriving (Show)

instance FromJSON Sync where
    parseJSON (Object v) = Sync <$> (v .: "configInSync")
    parseJSON _          = mzero

