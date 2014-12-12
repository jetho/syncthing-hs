
{-# LANGUAGE OverloadedStrings #-}

module Network.Syncthing.Get.Connections
    ( Connection(..)
    , connections
    ) where

import           Control.Applicative            ((<$>), (<*>))
import           Control.Monad                  (MonadPlus (mzero))
import           Data.Aeson                     (FromJSON, Value (..),
                                                 parseJSON, (.:))
import qualified Data.Map                       as M
import           Data.Text                      (Text)
import           Data.Time.Clock                (UTCTime)

import           Network.Syncthing.Common.Types
import           Network.Syncthing.Query
import           Network.Syncthing.Types
import           Network.Syncthing.Utils        (toUTC)


data Connection = Connection {
      getAt            :: Maybe UTCTime
    , getInBytesTotal  :: Integer
    , getOutBytesTotal :: Integer
    , getAddress       :: Text
    , getClientVersion :: Text
    } deriving (Show)

connections :: MonadSync m => SyncM m (M.Map DeviceId Connection)
connections = query $ getRequest { path = "/rest/connections" }

instance FromJSON Connection where
    parseJSON (Object v) =
        Connection <$> (toUTC <$> (v .:  "At"))
                   <*> (v .:  "InBytesTotal")
                   <*> (v .:  "OutBytesTotal")
                   <*> (v .:  "Address")
                   <*> (v .:  "ClientVersion")
    parseJSON _          = mzero

