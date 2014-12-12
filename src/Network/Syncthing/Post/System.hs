
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Network.Syncthing.Post.System
    ( SystemMsg(..)
    , restart
    , shutdown
    , reset
    , upgrade
    ) where

import           Control.Applicative     ((<$>))
import           Control.Monad           (MonadPlus (mzero), (>=>), join)
import           Data.Aeson              (FromJSON, Value (..), parseJSON, (.:))
import           Data.Text               (Text)
import           Network.Syncthing.Query
import           Network.Syncthing.Types


data SystemMsg
    = Restarting
    | ShuttingDown
    | ResettingFolders
    deriving (Show)

querySystemMsg :: MonadSync m => SyncRequest -> SyncM m (Maybe SystemMsg)
querySystemMsg = queryMaybe >=> return . join

restart :: MonadSync m => SyncM m (Maybe SystemMsg)
restart = querySystemMsg $ postRequest { path = "/rest/restart" }

shutdown :: MonadSync m => SyncM m (Maybe SystemMsg)
shutdown = querySystemMsg $ postRequest { path = "/rest/shutdown" }

reset :: MonadSync m => SyncM m (Maybe SystemMsg)
reset = querySystemMsg $ postRequest { path = "/rest/reset" }

upgrade :: MonadSync m => SyncM m (Maybe SystemMsg)
upgrade = querySystemMsg $ postRequest { path = "/rest/upgrade" }

instance FromJSON (Maybe SystemMsg) where
    parseJSON (Object v) = decodeSystemMsg <$> (v .: "ok")
    parseJSON _          = mzero

decodeSystemMsg :: Text -> Maybe SystemMsg
decodeSystemMsg = flip lookup
    [ ("restarting",        Restarting)
    , ("shutting down",     ShuttingDown)
    , ("resetting folders", ResettingFolders)
    ]

