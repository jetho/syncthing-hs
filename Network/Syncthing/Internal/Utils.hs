
{-# LANGUAGE OverloadedStrings #-}

module Network.Syncthing.Internal.Utils
    ( parseAddr
    , encodeAddr
    , toUTC
    , fromUTC
    ) where

import           Control.Applicative                     ((<$>))
import           Control.Arrow                           (second, (***))
import           Data.Maybe                              (fromMaybe)
import qualified Data.Text                               as T
import           Data.Time.Clock                         (UTCTime)
import           Data.Time.Format                        (formatTime, parseTime)
import           System.Locale                           (defaultTimeLocale)
import           Text.Regex.Posix                        ((=~))

import           Network.Syncthing.Internal.Config
import           Network.Syncthing.Types.Common


-- | Parse server string (SERVER:PORT) into an address type.
parseAddr :: Server -> Addr
parseAddr s =
    if serverString =~ ("^[^:]+:[0-9]+$" :: String)
        then mapAddr . split (== ':') $ serverString
        else (T.pack serverString, Nothing)
  where
    serverString = T.unpack s
    mapAddr :: (String, String) -> Addr
    mapAddr = T.pack *** (Just . read)

split :: (Char -> Bool) -> String -> (String, String)
split p = (second $ drop 1) . break p

-- | Generate server string.
encodeAddr :: Addr -> Server
encodeAddr (host, maybePort) = host `T.append` portSuffix
  where
    portSuffix = fromMaybe "" portPart
    portPart   = T.pack . (:) ':' . show <$> maybePort

-- | Convert time string to UTCTime type.
toUTC :: String -> Maybe UTCTime
toUTC = parseTime defaultTimeLocale "%FT%X%Q%z"

-- | Generate time string from UTC.
fromUTC :: UTCTime -> String
fromUTC = formatTime defaultTimeLocale "%FT%X%Q%z"

