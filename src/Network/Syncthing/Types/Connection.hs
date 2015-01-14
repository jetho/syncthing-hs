
{-# LANGUAGE OverloadedStrings #-}

module Network.Syncthing.Types.Connection
    ( Connection(..)
    ) where

import           Control.Applicative              ((<$>), (<*>))
import           Control.Monad                    (MonadPlus (mzero))
import           Data.Aeson                       (FromJSON, Value (..), parseJSON, (.:))
import           Data.Text                        (Text)
import           Data.Time.Clock                  (UTCTime)

import           Network.Syncthing.Types.Common   (Addr)
import           Network.Syncthing.Utils          (parseAddr, toUTC)


data Connection = Connection {
      getAt            :: Maybe UTCTime
    , getInBytesTotal  :: Integer
    , getOutBytesTotal :: Integer
    , getAddress       :: Addr
    , getClientVersion :: Text
    } deriving (Show)

instance FromJSON Connection where
    parseJSON (Object v) =
        Connection <$> (toUTC <$> (v .:  "At"))
                   <*> (v .:  "InBytesTotal")
                   <*> (v .:  "OutBytesTotal")
                   <*> (parseAddr <$> (v .:  "Address"))
                   <*> (v .:  "ClientVersion")
    parseJSON _          = mzero

