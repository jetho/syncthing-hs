
{-# LANGUAGE OverloadedStrings #-}

module Network.Syncthing.Common.Ignores
    ( Ignore(..)
    ) where

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad       (MonadPlus (mzero))
import           Data.Aeson          (FromJSON, Value (..), parseJSON, (.:?))
import           Data.Text           (Text)


data Ignore = Ignore {
      getIgnores  :: Maybe [Text]
    , getPatterns :: Maybe [Text]
    } deriving (Show)

instance FromJSON Ignore where
    parseJSON (Object v) =
        Ignore <$> (v .:? "ignore")
               <*> (v .:? "patterns")
    parseJSON _          = mzero

