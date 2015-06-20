
{-# LANGUAGE OverloadedStrings #-}

module Network.Syncthing.Types.SystemMsg
    ( SystemMsg(..)
    ) where

import           Control.Applicative  ((<$>), (*>), (<|>), pure)
import           Control.Monad        (MonadPlus (mzero))
import           Data.Aeson           (FromJSON, Value (..), parseJSON, (.:))
import qualified Data.Attoparsec.Text as A
import           Data.Maybe           (fromMaybe)
import           Data.Text            (Text)

import           Network.Syncthing.Types.Common (FolderName)


-- | System messages.
data SystemMsg
    = Restarting
    | ShuttingDown
    | ResettingDatabase
    | ResettingFolder Text
    | OtherSystemMsg Text
    deriving (Eq, Show)

instance FromJSON SystemMsg where
    parseJSON (Object v) = parseSystemMsg <$> (v .: "ok")
    parseJSON _          = mzero

parseSystemMsg :: Text -> SystemMsg
parseSystemMsg msg = 
    fromMaybe (OtherSystemMsg msg) $ A.maybeResult $ A.parse systemMsgParser msg

systemMsgParser :: A.Parser SystemMsg
systemMsgParser = 
        "restarting" *> pure Restarting
    <|> "shutting down" *> pure ShuttingDown
    <|> "resetting database" *> pure ResettingDatabase
    <|> ResettingFolder <$> ("resetting folder " *> A.takeText)

