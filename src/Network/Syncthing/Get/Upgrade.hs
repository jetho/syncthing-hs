
{-# LANGUAGE OverloadedStrings #-}

module Network.Syncthing.Get.Upgrade
    ( Upgrade(..)
    , upgrade
    ) where

import           Control.Applicative     ((<$>), (<*>))
import           Control.Monad           (MonadPlus (mzero))
import           Data.Aeson              (FromJSON, Value (..), parseJSON, (.:))
import           Data.Text               (Text)
import           Network.Syncthing.Query
import           Network.Syncthing.Types


data Upgrade = Upgrade {
      getLatest  :: Text
    , getNewer   :: Bool
    , getRunning :: Text
    } deriving (Show)

upgrade :: MonadSync m => SyncM m Upgrade
upgrade = query $ getRequest { path   = "/rest/upgrade" }

instance FromJSON Upgrade where
    parseJSON (Object v) =
        Upgrade <$> (v .: "latest")
                <*> (v .: "newer")
                <*> (v .: "running")
    parseJSON _          = mzero

