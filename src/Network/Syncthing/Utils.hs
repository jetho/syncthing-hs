
{-# LANGUAGE OverloadedStrings #-}

module Network.Syncthing.Utils
    ( decodeError
    , decodeDeviceError
    , toUTC
    , parseAddr
    ) where

import           Control.Arrow                    (second, (***))
import           Data.ByteString.Lazy             (ByteString)
import           Data.ByteString.Lazy.Char8       (unpack)
import           Data.List                        (find)
import           Data.Maybe                       (fromMaybe)
import qualified Data.Text                        as T (Text, pack, unpack)
import           Data.Time.Clock                  (UTCTime)
import           Data.Time.Format                 (parseTime)
import           System.Locale                    (defaultTimeLocale)
import           Text.Regex.Posix                 ((=~))

import           Network.Syncthing.Internal.Types
import           Network.Syncthing.Types.Common


deviceIdLength, deviceIdCheckDigit :: String
deviceIdLength     = "device ID invalid: incorrect length"
deviceIdCheckDigit = "check digit incorrect"

decodeError :: ByteString -> Maybe SyncError
decodeError =
      fmap snd
    . flip find errorPatterns
    . (\msg patTup -> msg =~ fst patTup)
    . unpack
  where
    errorPatterns :: [(String, SyncError)]
    errorPatterns =
        [ ("CSRF Error", CSRFError)
        , ("Not Authorized", NotAuthorized)
        , ("404 page not found", NotFound)
        , (deviceIdLength, InvalidDeviceId IncorrectLength)
        , (deviceIdCheckDigit, InvalidDeviceId IncorrectCheckDigit)
        , ("no such folder", NoSuchFolder )
        , ("Folder .*? does not exist", NoSuchFolder )
        ]

decodeDeviceError :: T.Text -> DeviceError
decodeDeviceError msg =
    fromMaybe (OtherDeviceError msg) $
        lookup (T.unpack msg)
               [ (deviceIdLength, IncorrectLength)
               , (deviceIdCheckDigit, IncorrectCheckDigit)
               ]

toUTC :: String -> Maybe UTCTime
toUTC = parseTime defaultTimeLocale "%FT%X%Q%z"

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

