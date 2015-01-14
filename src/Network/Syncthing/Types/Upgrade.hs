
{-# LANGUAGE OverloadedStrings #-}

module Network.Syncthing.Types.Upgrade
    ( Upgrade(..)
    ) where

import           Control.Applicative              ((<$>), (<*>))
import           Control.Monad                    (MonadPlus (mzero))
import           Data.Aeson                       (FromJSON, Value (..), parseJSON, (.:))
import           Data.Text                        (Text)


data Upgrade = Upgrade {
      getLatest  :: Text
    , getNewer   :: Bool
    , getRunning :: Text
    } deriving (Show)

instance FromJSON Upgrade where
    parseJSON (Object v) =
        Upgrade <$> (v .: "latest")
                <*> (v .: "newer")
                <*> (v .: "running")
    parseJSON _          = mzero

