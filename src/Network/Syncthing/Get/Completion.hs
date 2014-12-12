
{-# LANGUAGE OverloadedStrings #-}

module Network.Syncthing.Get.Completion
    ( completion
    ) where

import           Control.Applicative            ((<$>))
import           Control.Monad                  (MonadPlus (mzero))
import           Data.Aeson                     (FromJSON, Value (..), parseJSON, (.:))
import           Data.Text                      ()
import           Network.Syncthing.Common.Types (DeviceId, Folder)
import           Network.Syncthing.Query
import           Network.Syncthing.Types


newtype Completion = Completion { getCompletion :: Int } deriving (Show)

completion :: MonadSync m => DeviceId -> Folder -> SyncM m Int
completion device folder = getCompletion <$> completion'
  where
    completion' = query $ getRequest { path   = "/rest/completion"
                                     , params = [ ("device", device) 
                                                , ("folder", folder) ]
                                     }

instance FromJSON Completion where
    parseJSON (Object v) = Completion <$> (v .: "completion")
    parseJSON _          = mzero

