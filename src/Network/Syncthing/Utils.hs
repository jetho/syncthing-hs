
{-# LANGUAGE OverloadedStrings          #-}

module Network.Syncthing.Utils
    ( decodeError
    , toUTC
    ) where

import           Data.ByteString.Lazy    (ByteString)
import           Data.Time.Clock         (UTCTime)
import           Data.Time.Format        (parseTime)
import           System.Locale           (defaultTimeLocale)

import           Network.Syncthing.Types


decodeError :: ByteString -> Maybe SyncError
decodeError = flip lookup
    [ ("CSRF Error\n",                          CSRFError)
    , ("Not Authorized\n",                      NotAuthorized)
    , ("404 page not found\n",                  NotFound)
    , ("device ID invalid: incorrect length\n", InvalidDeviceId IncorrectLength)
    , ("check digit incorrect\n",               InvalidDeviceId IncorrectCheckDigit)
    ]

toUTC :: String -> Maybe UTCTime
toUTC = parseTime defaultTimeLocale "%FT%X%Q%z"

