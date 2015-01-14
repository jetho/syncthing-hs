
{-# LANGUAGE OverloadedStrings #-}

module Network.Syncthing.Types.CacheEntry
    ( CacheEntry(..)
    ) where

import           Control.Applicative              ((<$>), (<*>))
import           Control.Monad                    (MonadPlus (mzero))
import           Data.Aeson                       (FromJSON, Value (..), parseJSON, (.:))
import           Data.Text                        ()
import           Data.Time.Clock                  (UTCTime)

import           Network.Syncthing.Types.Common   (Addr)
import           Network.Syncthing.Utils          (parseAddr, toUTC)


data CacheEntry = CacheEntry {
      getAddr :: Addr
    , getSeen :: Maybe UTCTime
    } deriving (Show)

instance FromJSON CacheEntry where
    parseJSON (Object v) =
        CacheEntry <$> (parseAddr <$> (v .:  "Address"))
                   <*> (toUTC <$> (v .:  "Seen"))
    parseJSON _          = mzero

