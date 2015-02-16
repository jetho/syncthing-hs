
{-# LANGUAGE OverloadedStrings #-}

module Network.Syncthing.Types.Ignore
    ( Ignore(..)
    ) where

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad       (MonadPlus (mzero))
import           Data.Aeson          (FromJSON, Value (..), parseJSON, (.:?))
import           Data.Text           (Text)


-- | Contains the ignores list and a list of all compiled ignore patterns.
data Ignore = Ignore {
      getIgnores  :: Maybe [Text]
    , getPatterns :: Maybe [Text]
    } deriving (Show)

instance FromJSON Ignore where
    parseJSON (Object v) =
        Ignore <$> (v .:? "ignore")
               <*> (v .:? "patterns")
    parseJSON _          = mzero

