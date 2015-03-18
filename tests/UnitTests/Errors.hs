
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module UnitTests.Errors
    ( errorUnits
    ) where

import           Control.Monad.Catch.Pure
import           Control.Monad.Trans.Class  (lift)
import qualified Data.ByteString            as BS
import           Data.Default               (def)
import qualified Data.Text                  as T
import           Network.HTTP.Client
import           Network.HTTP.Types
import           Test.Tasty
import           Test.Tasty.HUnit

import           Network.Syncthing
import qualified Network.Syncthing.Get      as Get
import           Network.Syncthing.Internal



-------------- Test Infrastructure --------------

type CatchM        = SyncM Catch
type CatchResult a = Either SomeException (SyncResult a)


instance MonadSync Catch where
    getMethod  o s   = return "{\"invalidKey\":\"pong\"}"
    postMethod o s p = return "{\"invalidKey\":\"pong\"}"

instance MonadThrow CatchM where
    throwM = SyncM . throwM 

requestWithException :: Exception ex => ex -> CatchM a -> SyncM Catch a
requestWithException ex action = throwM ex >> action

runRequest :: SyncConfig -> CatchM a -> CatchResult a
runRequest cfg action = runCatch $ syncthingM cfg action



-------------- Helpers --------------

pingWithEx :: Exception ex => ex -> CatchResult T.Text
pingWithEx ex = runRequest defaultConfig $ requestWithException ex Get.ping

statusCodeEx :: Status -> BS.ByteString -> HttpException
statusCodeEx status msg =
    StatusCodeException status [("X-Response-Body-Start", msg)] def

testParseEx =
    testCase "Handle ParseError" $
        assertBool "JSON parse errors are handled correctly" $
            isHandled $ runRequest defaultConfig Get.ping
  where
    isHandled (Right (Left (ParseError _))) = True
    isHandled _                             = False

testHandledEx :: Exception ex => SyncError -> ex -> TestTree
testHandledEx errType ex =
    testCase ("Handle " ++ show errType) $
        assertBool "Exception is handled correctly" $
            isHandled (pingWithEx ex)
  where
    isHandled (Right (Left err)) = errType == err
    isHandled _                  = False

testUnhandledEx :: Exception ex => ex -> TestTree
testUnhandledEx ex =
    testCase ("Unhandled Exception: " ++ show ex) $
        assertBool "Exception is not handled" $
            isNotHandled (pingWithEx ex)
  where
    isNotHandled (Left _) = True
    isNotHandled _        = False

testDeviceError :: DeviceError -> T.Text -> TestTree
testDeviceError errType errMsg =
    testCase ("Decoding " ++ show errType) $
        assertEqual description errType (decodeDeviceError errMsg)
  where
    description = concat [show (T.unpack errMsg), " decodes to ", show errType]



-------------- Test Suite --------------

errorUnits :: TestTree
errorUnits = testGroup "Error Handling Unit Tests"
    [ testGroup "Handled Exceptions"
        [ testParseEx
        , testHandledEx CSRFError $ statusCodeEx status403 "CSRF Error"
        , testHandledEx NotAuthorized $ statusCodeEx status401 "Not Authorized"
        , testHandledEx NotFound $ statusCodeEx status404 "404 page not found"
        , testHandledEx NoSuchFolder $ statusCodeEx status500 "no such folder"
        , testHandledEx (InvalidDeviceId IncorrectLength)
            (statusCodeEx status500 "device ID invalid: incorrect length")
        , testHandledEx (InvalidDeviceId IncorrectCheckDigit)
            (statusCodeEx status500 "check digit incorrect")
        ]
    , testGroup "Unhandled Exceptions"
        [ testUnhandledEx $ statusCodeEx status500 "unknown Error"
        , testUnhandledEx TooManyRetries
        , testUnhandledEx ResponseTimeout
        , testUnhandledEx $ FailedConnectionException "127.0.0.1" 8080
        ]
    , testGroup "Decoding Device Errors"
        [ testDeviceError IncorrectLength "device ID invalid: incorrect length"
        , testDeviceError IncorrectCheckDigit "check digit incorrect"
        ]
    ]

