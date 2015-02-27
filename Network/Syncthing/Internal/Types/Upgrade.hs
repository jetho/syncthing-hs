
{-# LANGUAGE OverloadedStrings #-}

module Network.Syncthing.Internal.Types.Upgrade
    ( Upgrade(..)
    ) where

import           Control.Applicative              ((<$>), (<*>))
import           Control.Monad                    (MonadPlus (mzero))
import           Data.Aeson                       (FromJSON, Value (..), parseJSON, (.:))
import           Data.Text                        (Text)


-- | Information about the current software version and upgrade possibilities.
data Upgrade = Upgrade {
      getLatest  :: Text
    , getNewer   :: Bool
    , getRunning :: Text
    } deriving (Eq, Show)

instance FromJSON Upgrade where
    parseJSON (Object v) =
        Upgrade <$> (v .: "latest")
                <*> (v .: "newer")
                <*> (v .: "running")
    parseJSON _          = mzero

