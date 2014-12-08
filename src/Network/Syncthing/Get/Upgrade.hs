
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

upgrade :: MonadST m => SyncthingM m Upgrade
upgrade = query $
              SyncthingRequest {
                path   = "/rest/upgrade"
              , method = Get
              , params = []
              }

instance FromJSON Upgrade where
    parseJSON (Object v) =
        Upgrade <$> (v .: "latest")
                <*> (v .: "newer")
                <*> (v .: "running")
    parseJSON _          = mzero

