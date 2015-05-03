
{-# LANGUAGE OverloadedStrings #-}

module Network.Syncthing.Types.FolderInfo
    ( FolderInfo(..)
    , LastFile(..)
    ) where

import           Control.Applicative              ((<$>), (<*>))
import           Control.Monad                    (MonadPlus (mzero))
import           Data.Aeson                       (FromJSON, Value (..), parseJSON, (.:))
import           Data.Text                        (Text)
import           Data.Time.Clock                  (UTCTime)

import           Network.Syncthing.Internal.Utils (toUTC)


-- | Contains general statistics about folders.
data FolderInfo = FolderInfo {
      getLastFile :: LastFile
    } deriving (Eq, Show)

-- | Information about the last synced file.
data LastFile = LastFile {
      getFileName  :: Text
    , getSyncedAt  :: Maybe UTCTime
    } deriving (Eq, Show)

instance FromJSON FolderInfo where
    parseJSON (Object v) = FolderInfo <$> (v .:  "lastFile")
    parseJSON _          = mzero

instance FromJSON LastFile where
    parseJSON (Object v) =
        LastFile <$> (v .: "filename")
                 <*> (toUTC <$> (v .: "at"))
    parseJSON _          = mzero

