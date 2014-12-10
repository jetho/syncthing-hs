
{-# LANGUAGE OverloadedStrings #-}

module Network.Syncthing.Get.System
    ( System(..)
    , system
    ) where

import           Control.Applicative     ((<$>), (<*>))
import           Control.Monad           (MonadPlus (mzero))
import           Data.Aeson              (FromJSON, Value (..), parseJSON, (.:), (.:?))
import           Data.Text               (Text)
import           Network.Syncthing.Query
import           Network.Syncthing.Types


data System = System {
      getAlloc         :: Integer
    , getCpuPercent    :: Double
    , getExtAnnounceOK :: Maybe Bool
    , getGoRoutines    :: Int
    , getMyId          :: Text
    , getSys           :: Integer
    } deriving (Show)

system :: MonadSync m => SyncM m System
system = query $
             SyncthingRequest {
               path   = "/rest/system"
             , method = Get
             , params = []
             }

instance FromJSON System where
    parseJSON (Object v) =
        System <$> (v .:  "alloc")
               <*> (v .:  "cpuPercent")
               <*> (v .:? "extAnnounceOK")
               <*> (v .:  "goroutines")
               <*> (v .:  "myID")
               <*> (v .:  "sys")
    parseJSON _          = mzero

