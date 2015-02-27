
{-# LANGUAGE OverloadedStrings #-}

module Network.Syncthing.Internal.Types.SystemMsg
    ( SystemMsg(..)
    ) where

import           Control.Applicative ((<$>))
import           Control.Monad       (MonadPlus (mzero))
import           Data.Aeson          (FromJSON, Value (..), parseJSON, (.:))
import           Data.Maybe          (fromMaybe)
import           Data.Text           (Text)


-- | System messages.
data SystemMsg
    = Restarting
    | ShuttingDown
    | ResettingFolders
    | OtherSystemMsg Text
    deriving (Eq, Show)

instance FromJSON SystemMsg where
    parseJSON (Object v) = decodeSystemMsg <$> (v .: "ok")
    parseJSON _          = mzero

decodeSystemMsg :: Text -> SystemMsg
decodeSystemMsg msg = fromMaybe (OtherSystemMsg msg) maybeMsg
  where 
    maybeMsg = lookup msg
        [ ("restarting",        Restarting)
        , ("shutting down",     ShuttingDown)
        , ("resetting folders", ResettingFolders)
        ]

