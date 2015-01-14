
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Syncthing.Types.SystemMsg
    ( SystemMsg(..)
    ) where

import           Control.Applicative              ((<$>))
import           Control.Monad                    (MonadPlus (mzero))
import           Data.Aeson                       (FromJSON, Value (..), parseJSON, (.:))
import           Data.Text                        (Text)


data SystemMsg
    = Restarting
    | ShuttingDown
    | ResettingFolders
    deriving (Show)

instance FromJSON (Maybe SystemMsg) where
    parseJSON (Object v) = decodeSystemMsg <$> (v .: "ok")
    parseJSON _          = mzero

decodeSystemMsg :: Text -> Maybe SystemMsg
decodeSystemMsg = flip lookup
    [ ("restarting",        Restarting)
    , ("shutting down",     ShuttingDown)
    , ("resetting folders", ResettingFolders)
    ]

