
{-# LANGUAGE OverloadedStrings #-}

module Network.Syncthing.Get.Model
    ( Model(..)
    , ModelState(..)
    , model
    ) where

import           Control.Applicative              ((<$>), (<*>))
import           Control.Monad                    (MonadPlus (mzero))
import           Data.Aeson                       (FromJSON, Value (..), parseJSON, (.:))
import           Data.Text                        (Text)
import           Data.Time.Clock                  (UTCTime)

import           Network.Syncthing.Common.Types
import           Network.Syncthing.Internal.Query
import           Network.Syncthing.Internal.Types
import           Network.Syncthing.Utils          (toUTC)


data ModelState
    = Idle
    | Scanning
    | Cleaning
    | Syncing
    deriving (Eq, Show)

data Model = Model {
      getGlobalBytes   :: Integer
    , getGlobalDeleted :: Integer
    , getGlobalFiles   :: Integer
    , getInSyncBytes   :: Integer
    , getInSyncFiles   :: Integer
    , getLocalBytes    :: Integer
    , getLocalDeleted  :: Integer
    , getLocalFiles    :: Integer
    , getNeedBytes     :: Integer
    , getNeedFiles     :: Integer
    , getState         :: Maybe ModelState
    , getStateChanged  :: Maybe UTCTime
    , getInvalid       :: Maybe Text
    , getModelVersion  :: Int
    } deriving (Show)

model :: MonadSync m => FolderName -> SyncM m Model
model folder = query $ getRequest { path   = "/rest/model"
                                  , params = [("folder", folder)]
                                  }

instance FromJSON Model where
    parseJSON (Object v) =
        Model <$> (v .: "globalBytes")
              <*> (v .: "globalDeleted")
              <*> (v .: "globalFiles")
              <*> (v .: "inSyncBytes")
              <*> (v .: "inSyncFiles")
              <*> (v .: "localBytes")
              <*> (v .: "localDeleted")
              <*> (v .: "localFiles")
              <*> (v .: "needBytes")
              <*> (v .: "needFiles")
              <*> (decodeModelState <$> (v .: "state"))
              <*> (toUTC <$> (v .: "stateChanged"))
              <*> (decodeInvalid <$> (v .: "invalid"))
              <*> (v .: "version")
    parseJSON _          = mzero

decodeModelState :: Text -> Maybe ModelState
decodeModelState = flip lookup
    [ ("idle", Idle)
    , ("scanning", Scanning)
    , ("cleaning", Cleaning)
    , ("syncing", Syncing)
    ]

decodeInvalid :: Text -> Maybe Text
decodeInvalid "" = Nothing
decodeInvalid s  = Just s

