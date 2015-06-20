
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}

module Network.Syncthing.Internal.Error
    ( DeviceError(..)
    , SyncError(..)
    , syncErrHandler
    , decodeDeviceError
    , parseError
    ) where

import           Control.Applicative             ((<$>), (*>), pure)
import           Control.Monad                   ((<=<))
import           Control.Monad.Catch             (Exception, MonadThrow, throwM)
import qualified Data.Attoparsec.ByteString      as A
import qualified Data.ByteString.Char8           as BS
import           Data.List                       (find)
import           Data.Maybe                      (fromMaybe)
import qualified Data.Text                       as T
import           Data.Text.Encoding              (decodeUtf8, encodeUtf8)
import           Data.Typeable                   (Typeable)
import qualified Network.HTTP.Client             as HTTP

import           Network.Syncthing.Types.Common  (FolderName)


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
    | UnknownFolder FolderName
    deriving (Typeable, Eq, Show)

instance Exception SyncError


syncErrHandler :: MonadThrow m => HTTP.HttpException -> m (Either SyncError a)
syncErrHandler e@(HTTP.StatusCodeException _ headers _) =
    maybe (throwM e) (return . Left) $ extractErr headers
  where
    extractErr = parseError <=< lookup "X-Response-Body-Start"
syncErrHandler unhandledErr = throwM unhandledErr

deviceIdLength, deviceIdCheckDigit :: BS.ByteString
deviceIdLength     = "device ID invalid: incorrect length"
deviceIdCheckDigit = "check digit incorrect"

parseError :: BS.ByteString -> Maybe SyncError
parseError msg =
    case A.parseOnly errorParser msg of
        Left _  -> Nothing
        Right m -> Just m

errorParser :: A.Parser SyncError
errorParser = A.choice
    [ A.string "CSRF Error" *> pure CSRFError
    , A.string "Not Authorized" *> pure NotAuthorized
    , A.string "404 page not found" *> pure NotFound
    , A.string deviceIdLength *> pure (InvalidDeviceId IncorrectLength)
    , A.string deviceIdCheckDigit *> pure (InvalidDeviceId IncorrectCheckDigit)
    , A.string "no such folder" *> pure NoSuchFolder
    , A.string "Folder " >> A.manyTill A.anyWord8 (A.string "does not exist") 
        *> pure NoSuchFolder
    , UnknownFolder . decodeUtf8 <$> 
        (A.string "Unknown folder " *> A.takeByteString)
    ]

decodeDeviceError :: T.Text -> DeviceError
decodeDeviceError msg =
    fromMaybe (OtherDeviceError msg) $
        lookup (encodeUtf8 msg)
               [ (deviceIdLength, IncorrectLength)
               , (deviceIdCheckDigit, IncorrectCheckDigit)
               ]

