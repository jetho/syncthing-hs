
{-# LANGUAGE OverloadedStrings #-}

module Network.Syncthing.Types.Need
    ( Need(..)
    ) where

import           Control.Applicative              ((<$>), (<*>))
import           Control.Monad                    (MonadPlus (mzero))
import           Data.Aeson                       (FromJSON, Value (..), parseJSON, (.:))

import           Network.Syncthing.Types.FileInfo (FileInfo)


-- | Contains lists of files which are needed by a device for becoming in
-- sync.
data Need = Need {
      getProgress :: [FileInfo]
    , getQueued   :: [FileInfo]
    , getRest     :: [FileInfo]
    } deriving (Eq, Show)

instance FromJSON Need where
    parseJSON (Object v) =
        Need <$> (v .: "progress")
             <*> (v .: "queued")
             <*> (v .: "rest")
    parseJSON _          = mzero

