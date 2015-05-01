
{-# LANGUAGE OverloadedStrings #-}

module Network.Syncthing.Types.DBFile
    ( DBFile(..)
    ) where

import           Control.Applicative              ((<$>), (<*>))
import           Control.Monad                    (MonadPlus (mzero))
import           Data.Aeson                       (FromJSON, Value (..), parseJSON, (.:))

import           Network.Syncthing.Types.Common   (Device)
import           Network.Syncthing.Types.FileInfo (FileInfo)


-- | Contains data available about a given file.
data DBFile = DBFile {
      getAvailability   :: [Device]
    , getGlobal         :: FileInfo
    , getLocal          :: FileInfo
    } deriving (Eq, Show)

instance FromJSON DBFile where
    parseJSON (Object v) =
        DBFile <$> (v .: "availability")
               <*> (v .: "global")
               <*> (v .: "local")
    parseJSON _          = mzero

