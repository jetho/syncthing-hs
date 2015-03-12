
{-# LANGUAGE OverloadedStrings #-}

module Network.Syncthing.Types.DirTree
    ( DirTree(..)
    ) where

import           Control.Applicative ((<$>))
import           Control.Monad       (MonadPlus (mzero))
import           Data.Aeson          (FromJSON, Value (..), parseJSON)
import qualified Data.Map            as M
import           Data.Text           (Text)


-- | A directory tree contains files or subdirectories.
data DirTree = DirTree {
      getDirTree :: M.Map Text DirTree
    }
    |
    File {
      getModTime  :: Integer    -- ^ file modification time
    , getFileSize :: Integer    -- ^ file size
    } deriving (Eq, Show)

instance FromJSON DirTree where
    parseJSON obj@(Object _) = DirTree <$> parseJSON obj
    parseJSON arr@(Array _)  = decodeFileInfo <$> parseJSON arr
    parseJSON _              = mzero


decodeFileInfo :: [Integer] -> DirTree
decodeFileInfo [modTime, fileSize] = File modTime fileSize
decodeFileInfo _                   = File 0 0

