
{-# LANGUAGE OverloadedStrings #-}

module Network.Syncthing.Utils
    ( decodeError
    , toUTC
    ) where

import           Data.ByteString.Lazy       (ByteString)
import           Data.ByteString.Lazy.Char8 (unpack)
import           Data.List                  (find)
import           Data.Time.Clock            (UTCTime)
import           Data.Time.Format           (parseTime)
import           Text.Regex.Posix           ((=~))
import           System.Locale              (defaultTimeLocale)

import           Network.Syncthing.Types


decodeError :: ByteString -> Maybe SyncError
decodeError = 
      fmap snd
    . flip find errorPatterns
    . (\msg -> \patTup -> msg =~ fst patTup)
    . unpack
  where
    errorPatterns :: [(String, SyncError)]
    errorPatterns =
        [ ("CSRF Error", CSRFError)
        , ("Not Authorized", NotAuthorized)
        , ("404 page not found", NotFound)
        , ("device ID invalid: incorrect length", InvalidDeviceId IncorrectLength)
        , ("check digit incorrect", InvalidDeviceId IncorrectCheckDigit)
        , ("no such folder", NoSuchFolder )
        , ("Folder .*? does not exist", NoSuchFolder )
        ]

toUTC :: String -> Maybe UTCTime
toUTC = parseTime defaultTimeLocale "%FT%X%Q%z"

