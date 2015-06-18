
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}

module Network.Syncthing.Internal.Error
    ( DeviceError(..)
    , SyncError(..)
    , syncErrHandler
    , decodeDeviceError
    , decodeError
    ) where

import           Control.Monad              ((<=<))
import           Control.Monad.Catch        (Exception, MonadThrow, throwM)
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.List                  (find)
import           Data.Maybe                 (fromMaybe)
import qualified Data.Text                  as T
import           Data.Typeable              (Typeable)
import qualified Network.HTTP.Client        as HTTP
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


syncErrHandler :: MonadThrow m => HTTP.HttpException -> m (Either SyncError a)
syncErrHandler e@(HTTP.StatusCodeException _ headers _) =
    maybe (throwM e) (return . Left) $ extractErr headers
  where
    extractErr = decodeError . BS.fromStrict <=< lookup "X-Response-Body-Start"
syncErrHandler unhandledErr = throwM unhandledErr

deviceIdLength, deviceIdCheckDigit :: String
deviceIdLength     = "device ID invalid: incorrect length"
deviceIdCheckDigit = "check digit incorrect"

decodeError :: BS.ByteString -> Maybe SyncError
decodeError =
      fmap snd
    . flip find errorPatterns
    . (\msg patTup -> msg =~ fst patTup)
    . BS.unpack
  where
    errorPatterns :: [(String, SyncError)]
    errorPatterns =
        [ ("CSRF Error", CSRFError)
        , ("Not Authorized", NotAuthorized)
        , ("404 page not found", NotFound)
        , (deviceIdLength, InvalidDeviceId IncorrectLength)
        , (deviceIdCheckDigit, InvalidDeviceId IncorrectCheckDigit)
        , ("no such folder", NoSuchFolder )
        , ("Folder .*? does not exist", NoSuchFolder)
        , ("Unknown folder", NoSuchFolder)
        ]

decodeDeviceError :: T.Text -> DeviceError
decodeDeviceError msg =
    fromMaybe (OtherDeviceError msg) $
        lookup (T.unpack msg)
               [ (deviceIdLength, IncorrectLength)
               , (deviceIdCheckDigit, IncorrectCheckDigit)
               ]

