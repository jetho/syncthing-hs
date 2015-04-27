
{-# LANGUAGE OverloadedStrings #-}

module Network.Syncthing.Types.Error
    ( Error(..)
    , Errors(..)
    ) where

import           Control.Applicative              ((<$>), (<*>))
import           Control.Monad                    (MonadPlus (mzero))
import           Data.Aeson                       (FromJSON, Value (..), parseJSON, (.:))
import           Data.Text                        (Text)
import           Data.Time.Clock                  (UTCTime)

import           Network.Syncthing.Internal.Utils (toUTC)


-- | An error message and its timestamp.
data Error = Error {
      getTime :: Maybe UTCTime
    , getMsg  :: Text
    } deriving (Eq, Show)

newtype Errors = Errors { getErrors :: [Error] }
                 deriving (Eq, Show)

instance FromJSON Error where
    parseJSON (Object v) =
        Error <$> (toUTC <$> (v .:  "time"))
              <*> (v .:  "error")
    parseJSON _          = mzero

instance FromJSON Errors where
    parseJSON (Object v) = Errors <$> (v .: "errors")
    parseJSON _          = mzero

