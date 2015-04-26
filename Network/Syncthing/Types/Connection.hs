
{-# LANGUAGE OverloadedStrings #-}

module Network.Syncthing.Types.Connection
    ( Connection(..)
    , Connections(..)
    ) where

import           Control.Applicative              ((<$>), (<*>))
import           Control.Monad                    (MonadPlus (mzero))
import           Data.Aeson                       (FromJSON, Value (..), parseJSON, (.:))
import qualified Data.Map                         as M
import           Data.Text                        (Text)
import           Data.Time.Clock                  (UTCTime)

import           Network.Syncthing.Types.Common   (Addr, Device)
import           Network.Syncthing.Internal.Utils (parseAddr, toUTC)


-- | Connection information and some associated metadata.
data Connection = Connection {
      getAt            :: Maybe UTCTime
    , getInBytesTotal  :: Integer
    , getOutBytesTotal :: Integer
    , getAddress       :: Addr
    , getClientVersion :: Text
    } deriving (Eq, Show)

instance FromJSON Connection where
    parseJSON (Object v) =
        Connection <$> (toUTC <$> (v .:  "at"))
                   <*> (v .:  "inBytesTotal")
                   <*> (v .:  "outBytesTotal")
                   <*> (parseAddr <$> (v .:  "address"))
                   <*> (v .:  "clientVersion")
    parseJSON _          = mzero

-- | Contains the list of current connections.
data Connections = Connections {
      getConnections    :: M.Map Device Connection
    , getTotal          :: Connection
    } deriving (Eq, Show)

instance FromJSON Connections where
    parseJSON (Object v) =
        Connections <$> (v .:  "connections")
                    <*> (v .:  "total")
    parseJSON _          = mzero

