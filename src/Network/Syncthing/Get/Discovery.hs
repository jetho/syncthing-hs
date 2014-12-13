
{-# LANGUAGE OverloadedStrings #-}

module Network.Syncthing.Get.Discovery
    ( CacheEntry(..)
    , discovery
    ) where

import           Control.Applicative            ((<$>), (<*>))
import           Control.Monad                  (MonadPlus (mzero))
import           Data.Aeson                     (FromJSON, Value (..), parseJSON, (.:))
import qualified Data.Map                       as M
import           Data.Text                      (Text)
import           Data.Time.Clock                (UTCTime)

import           Network.Syncthing.Common.Types
import           Network.Syncthing.Query
import           Network.Syncthing.Types
import           Network.Syncthing.Utils        (toUTC)


data CacheEntry = CacheEntry {
      getAddr    :: Text
    , getSeen    :: Maybe UTCTime
    } deriving (Show)

discovery :: MonadSync m => SyncM m (M.Map DeviceId [CacheEntry])
discovery = query $ getRequest { path = "/rest/discovery" }

instance FromJSON CacheEntry where
    parseJSON (Object v) =
        CacheEntry <$> (v .:  "Address")
                   <*> (toUTC <$> (v .:  "Seen"))
    parseJSON _          = mzero

