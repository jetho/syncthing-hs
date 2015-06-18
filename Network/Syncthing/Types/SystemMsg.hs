
{-# LANGUAGE OverloadedStrings #-}

module Network.Syncthing.Types.SystemMsg
    ( SystemMsg(..)
    ) where

import           Control.Applicative ((<$>))
import           Control.Monad       (MonadPlus (mzero))
import           Data.Aeson          (FromJSON, Value (..), parseJSON, (.:))
import           Data.List           (find)
import           Data.Maybe          (fromMaybe)
import           Data.Text           (Text, unpack)
import           Text.Regex.Posix    ((=~))


-- | System messages.
data SystemMsg
    = Restarting
    | ShuttingDown
    | ResettingDatabase
    | ResettingFolder
    | OtherSystemMsg Text
    deriving (Eq, Show)

instance FromJSON SystemMsg where
    parseJSON (Object v) = decodeSystemMsg <$> (v .: "ok")
    parseJSON _          = mzero

decodeSystemMsg :: Text -> SystemMsg
decodeSystemMsg msg =
      fromMaybe (OtherSystemMsg msg) $
                snd <$> find (\patTup -> unpack msg =~ fst patTup) msgPatterns
  where
    msgPatterns :: [(String, SystemMsg)]
    msgPatterns =
        [ ("restarting",            Restarting)
        , ("shutting down",         ShuttingDown)
        , ("resetting database",    ResettingDatabase)
        , ("resetting folder",      ResettingFolder)
        ]

