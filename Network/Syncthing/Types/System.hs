
{-# LANGUAGE OverloadedStrings #-}

module Network.Syncthing.Types.System
    ( System(..)
    ) where

import           Control.Applicative              ((<$>), (<*>))
import           Control.Monad                    (MonadPlus (mzero))
import           Data.Aeson                       (FromJSON, Value (..), parseJSON, (.:), (.:?))
import qualified Data.Map                         as M
import           Data.Text                        (Text)

import           Network.Syncthing.Types.Common   (Server)


-- | Information about the system status and resource usage.
data System = System {
      getAlloc         :: Integer
    , getCpuPercent    :: Double
    , getExtAnnounceOK :: Maybe (M.Map Server Bool)
    , getGoRoutines    :: Int
    , getMyId          :: Text
    , getSys           :: Integer
    } deriving (Eq, Show)

instance FromJSON System where
    parseJSON (Object v) =
        System <$> (v .:  "alloc")
               <*> (v .:  "cpuPercent")
               <*> (v .:? "extAnnounceOK")
               <*> (v .:  "goroutines")
               <*> (v .:  "myID")
               <*> (v .:  "sys")
    parseJSON _          = mzero

