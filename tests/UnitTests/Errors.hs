
{-# LANGUAGE OverloadedStrings #-}

module UnitTests.Errors
    ( errorUnits
    ) where

import           Data.ByteString.Lazy             (ByteString)
import qualified Data.Text                        as T
import           Test.Tasty
import           Test.Tasty.HUnit

import           Network.Syncthing.Internal.Error


testDecodeError :: SyncError -> ByteString -> TestTree
testDecodeError errType errMsg =
    testCase (show errType) $ decodeError errMsg @?= Just errType 

testDecodeDeviceError :: DeviceError -> T.Text -> TestTree
testDecodeDeviceError errType errMsg =
    testCase (show errType) $ decodeDeviceError errMsg @?= errType 


errorUnits :: TestTree
errorUnits = testGroup "Error Decoding Unit Tests" $
    map (uncurry testDecodeError)
    [ (CSRFError, "CSRF Error")
    , (NotAuthorized, "Not Authorized")
    , (NotFound, "404 page not found")
    , (NoSuchFolder, "no such folder")
    , ((InvalidDeviceId IncorrectLength), "device ID invalid: incorrect length")
    , ((InvalidDeviceId IncorrectCheckDigit), "check digit incorrect")
    ]
    ++
    map (uncurry testDecodeDeviceError) 
    [ (IncorrectLength, "device ID invalid: incorrect length")
    , (IncorrectCheckDigit, "check digit incorrect")
    ]

