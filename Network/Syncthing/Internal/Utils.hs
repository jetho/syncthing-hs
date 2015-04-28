
{-# LANGUAGE OverloadedStrings #-}

module Network.Syncthing.Internal.Utils
    ( parseAddr
    , encodeAddr
    , toUTC
    , fromUTC
    ) where

import           Control.Applicative            ((<$>))
import           Data.Maybe                     (fromMaybe)
import qualified Data.Text                      as T
import           Data.Time.Clock                (UTCTime)
import           Data.Time.Format               (formatTime, parseTime)
import           System.Locale                  (defaultTimeLocale)
import           Text.Regex.Posix               ((=~))

import           Network.Syncthing.Types.Common


-- | Parse server string (SERVER:PORT) into an address type.
parseAddr :: Server -> Addr
parseAddr s = addr (serverString =~ serverPat :: [[String]])
  where
    serverString     = T.unpack s
    serverPat        = "^\\[?([^]]+)\\]?:([0-9]+)$" :: String
    addr [[_, h, p]] = (T.pack h, Just $ read p)
    addr _           = (s, Nothing)

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

