{-# LANGUAGE OverloadedStrings #-}

module Network.Pulse.Common.Ping
    ( Ping(..)
    ) where

import           Control.Applicative ((<$>))
import           Control.Monad       (MonadPlus (mzero))
import           Data.Aeson
import           Data.Text

data Ping = Ping { getPing :: Text } deriving (Show)

