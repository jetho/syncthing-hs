
{-# OPTIONS_HADDOCK show-extensions, not-home #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}

module Network.Syncthing.Internal.Error
    ( DeviceError(..)
    , SyncError(..)
    , decodeDeviceError
    , decodeError
    ) where

import           Control.Exception          (Exception)
import           Data.ByteString.Lazy       (ByteString)
import           Data.ByteString.Lazy.Char8 (unpack)
import           Data.List                  (find)
import           Data.Maybe                 (fromMaybe)
import qualified Data.Text                  as T
import           Data.Typeable              (Typeable)
import           Text.Regex.Posix           ((=~))


data DeviceError =
      IncorrectLength
    | IncorrectCheckDigit
    | OtherDeviceError T.Text
    deriving (Eq, Show)

data SyncError =
      ParseError String
    | NotAuthorized
    | CSRFError
    | NotFound
    | InvalidDeviceId DeviceError
    | NoSuchFolder
    deriving (Typeable, Eq, Show)

instance Exception SyncError


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

