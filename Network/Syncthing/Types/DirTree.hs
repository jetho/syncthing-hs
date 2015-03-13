
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
data DirTree 
    = Dir {
        getDirContents :: M.Map Text DirTree
      }
    | File {
        getModTime  :: Integer    -- ^ file modification time
      , getFileSize :: Integer    -- ^ file size
      } 
    deriving (Eq, Show)

instance FromJSON DirTree where
    parseJSON obj@(Object _) = Dir <$> parseJSON obj
    parseJSON arr@(Array _)  = decodeFile <$> parseJSON arr
    parseJSON _              = mzero


decodeFile :: [Integer] -> DirTree
decodeFile [modTime, fileSize] = File modTime fileSize
decodeFile _                   = File 0 0

