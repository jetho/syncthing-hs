
{-# LANGUAGE OverloadedStrings #-}

module Network.Syncthing.Types.FileInfo
    ( FileInfo(..)
    ) where

import           Control.Applicative              ((<$>), (<*>))
import           Control.Monad                    (MonadPlus (mzero))
import           Data.Aeson                       (FromJSON, Value (..),
                                                   parseJSON, (.:), (.:?))
import           Data.Text                        (Text)
import           Data.Time.Clock                  (UTCTime)

import           Network.Syncthing.Internal.Utils (toUTC)


-- | All available information about a file.
data FileInfo = FileInfo {
      getName         :: Text
    , getFlags        :: Text
    , getModified     :: Maybe UTCTime
    , getFileVersion  :: [Text]
    , getLocalVersion :: Int
    , getSize         :: Integer
    , getNumBlocks    :: Maybe Int
    } deriving (Eq, Show)

instance FromJSON FileInfo where
    parseJSON (Object v) =
        FileInfo <$> (v .: "name")
                 <*> (v .: "flags")
                 <*> (toUTC <$> (v .: "modified"))
                 <*> (v .: "version")
                 <*> (v .: "localVersion")
                 <*> (v .: "size")
                 <*> (v .:? "numBlocks")
    parseJSON _          = mzero

