
{-# LANGUAGE OverloadedStrings #-}

module Network.Syncthing.Types.UsageReport
    ( UsageReport(..)
    ) where

import           Control.Applicative              ((<$>), (<*>))
import           Control.Monad                    (MonadPlus (mzero))
import           Data.Aeson                       (FromJSON, Value (..), parseJSON, (.:))
import           Data.Text                        (Text)


-- | Information about the data sent in the anonymous usage report.
data UsageReport = UsageReport {
      getFolderMaxFiles     :: Integer
    , getFolderMaxMiB       :: Integer 
    , getLongVersionR       :: Text
    , getMemorySize         :: Integer
    , getMemoryUsageMiB     :: Integer
    , getNumDevices         :: Int
    , getNumFolders         :: Int
    , getPlatform           :: Text
    , getSHA256Perf         :: Double
    , getTotFiles           :: Integer
    , getTotMiB             :: Integer
    , getUniqueId           :: Text
    , getVersionR           :: Text
    } deriving (Eq, Show)

instance FromJSON UsageReport where
    parseJSON (Object v) =
        UsageReport <$> (v .: "folderMaxFiles")
                    <*> (v .: "folderMaxMiB")
                    <*> (v .: "longVersion")
                    <*> (v .: "memorySize")
                    <*> (v .: "memoryUsageMiB")
                    <*> (v .: "numDevices")
                    <*> (v .: "numFolders")
                    <*> (v .: "platform")
                    <*> (v .: "sha256Perf")
                    <*> (v .: "totFiles")
                    <*> (v .: "totMiB")
                    <*> (v .: "uniqueID")
                    <*> (v .: "version")
    parseJSON _          = mzero

