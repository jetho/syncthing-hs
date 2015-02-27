
{-# LANGUAGE OverloadedStrings #-}

module Network.Syncthing.Internal.Types.System
    ( System(..)
    ) where

import           Control.Applicative              ((<$>), (<*>))
import           Control.Monad                    (MonadPlus (mzero))
import           Data.Aeson                       (FromJSON, Value (..), parseJSON, (.:), (.:?))
import           Data.Text                        (Text)


-- | Information about the system status and resource usage.
data System = System {
      getAlloc         :: Integer
    , getCpuPercent    :: Double
    , getExtAnnounceOK :: Maybe Bool
    , getGoRoutines    :: Int
    , getMyId          :: Text
    , getSys           :: Integer
    } deriving (Show)

instance FromJSON System where
    parseJSON (Object v) =
        System <$> (v .:  "alloc")
               <*> (v .:  "cpuPercent")
               <*> (v .:? "extAnnounceOK")
               <*> (v .:  "goroutines")
               <*> (v .:  "myID")
               <*> (v .:  "sys")
    parseJSON _          = mzero

