
{-# LANGUAGE OverloadedStrings #-}

module Network.Syncthing.Types.SystemMsg
    ( SystemMsg(..)
    ) where

import           Control.Applicative  ((<$>), (*>), (<|>))
import           Control.Monad        (MonadPlus (mzero))
import           Data.Aeson           (FromJSON, Value (..), parseJSON, (.:))
import           Data.Attoparsec.Text (Parser, parse, maybeResult, takeText)
import           Data.Maybe           (fromMaybe)
import           Data.Text            (Text)

import           Network.Syncthing.Types.Common (FolderName)


-- | System messages.
data SystemMsg
    = Restarting
    | ShuttingDown
    | ResettingDatabase
    | ResettingFolder FolderName
    | OtherSystemMsg Text
    deriving (Eq, Show)

instance FromJSON SystemMsg where
    parseJSON (Object v) = parseSystemMsg <$> (v .: "ok")
    parseJSON _          = mzero

parseSystemMsg :: Text -> SystemMsg
parseSystemMsg msg = 
    fromMaybe (OtherSystemMsg msg) $ maybeResult $ parse systemMsgParser msg

systemMsgParser :: Parser SystemMsg
systemMsgParser = 
        "restarting" *> pure Restarting
    <|> "shutting down" *> pure ShuttingDown
    <|> "resetting database" *> pure ResettingDatabase
    <|> ResettingFolder <$> ("resetting folder " *> takeText) 

