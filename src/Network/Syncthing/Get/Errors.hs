
{-# LANGUAGE OverloadedStrings #-}

module Network.Syncthing.Get.Errors
    ( Error(..)
    , errors
    ) where

import           Control.Applicative              ((<$>), (<*>))
import           Control.Monad                    (MonadPlus (mzero))
import           Data.Aeson                       (FromJSON, Value (..), parseJSON, (.:))
import           Data.Text                        (Text)
import           Data.Time.Clock                  (UTCTime)

import           Network.Syncthing.Internal.Query
import           Network.Syncthing.Internal.Types
import           Network.Syncthing.Utils          (toUTC)


data Error = Error {
      getTime :: Maybe UTCTime
    , getMsg  :: Text
    } deriving (Show)

newtype Errors = Errors { getErrors :: [Error] }

errors :: MonadSync m => SyncM m [Error]
errors = getErrors <$> errors'
  where
    errors' = query $ getRequest { path = "/rest/errors" }

instance FromJSON Error where
    parseJSON (Object v) =
        Error <$> (toUTC <$> (v .:  "Time"))
              <*> (v .:  "Error")
    parseJSON _          = mzero

instance FromJSON Errors where
    parseJSON (Object v) = Errors <$> (v .: "errors")
    parseJSON _          = mzero

