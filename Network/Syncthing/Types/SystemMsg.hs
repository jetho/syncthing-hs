
{-# LANGUAGE OverloadedStrings #-}

module Network.Syncthing.Types.SystemMsg
    ( SystemMsg(..)
    ) where

import           Control.Applicative  ((<$>), (*>), (<*), (<|>), pure)
import           Control.Monad        (MonadPlus (mzero))
import           Data.Aeson           (FromJSON, Value (..), parseJSON, (.:))
import qualified Data.Attoparsec.Text as A
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
    case A.parseOnly sysMsgParser msg of
        Left _  -> OtherSystemMsg msg
        Right m -> m

sysMsgParser :: A.Parser SystemMsg
sysMsgParser = 
        "restarting" *> pure Restarting
    <|> "shutting down" *> pure ShuttingDown
    <|> "resetting database" *> pure ResettingDatabase
    <|> ResettingFolder <$> ("resetting folder " *> A.takeText)

