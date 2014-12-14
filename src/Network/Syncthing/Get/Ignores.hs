
{-# LANGUAGE OverloadedStrings #-}

module Network.Syncthing.Get.Ignores
    ( Ignore(..)
    , ignores
    ) where

import           Control.Applicative            ((<$>), (<*>))
import           Control.Monad                  (MonadPlus (mzero))
import           Data.Aeson                     (FromJSON, Value (..), parseJSON, (.:))
import           Data.Text                      (Text)

import           Network.Syncthing.Common.Types
import           Network.Syncthing.Query
import           Network.Syncthing.Types


data Ignore = Ignore {
      getIgnores  :: Maybe [Text]
    , getPatterns :: Maybe [Text]
    } deriving (Show)

ignores :: MonadSync m => Folder -> SyncM m Ignore
ignores folder = query $ getRequest { path = "/rest/ignores" 
                                    , params = [("folder", folder)]
                                    }

instance FromJSON Ignore where
    parseJSON (Object v) =
        Ignore <$> (v .: "ignore")
               <*> (v .: "patterns")
    parseJSON _          = mzero

