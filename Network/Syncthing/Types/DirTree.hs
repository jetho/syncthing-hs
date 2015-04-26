
{-# LANGUAGE OverloadedStrings #-}

module Network.Syncthing.Types.DirTree
    ( DirTree(..)
    ) where

import           Control.Applicative              ((<$>), (<*>))
import           Control.Monad                    (MonadPlus (mzero))
import           Data.Aeson                       (FromJSON, Value (..),
                                                   parseJSON)
import qualified Data.Map                         as M
import           Data.Text                        (Text)
import           Data.Time.Clock                  (UTCTime)
import           Data.Vector                      ((!))

import           Network.Syncthing.Internal.Utils (toUTC)


-- | A directory tree contains files or subdirectories.
data DirTree
    = Dir {
        getDirContents :: M.Map Text DirTree
      }
    | File {
        getModTime  :: Maybe UTCTime    -- ^ file modification time
      , getFileSize :: Integer          -- ^ file size
      }
    deriving (Eq, Show)

instance FromJSON DirTree where
    parseJSON obj@(Object _) = Dir  <$> parseJSON obj
    parseJSON (Array v)      = File <$> (toUTC <$> parseJSON (v ! 0))
                                    <*> parseJSON (v ! 1)
    parseJSON _              = mzero

