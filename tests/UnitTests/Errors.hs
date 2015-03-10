
{-# LANGUAGE OverloadedStrings #-}

module UnitTests.Errors
    ( errorUnits
    ) where

import qualified Data.ByteString.Lazy.Char8       as BS 
import qualified Data.Text                        as T
import           Test.Tasty
import           Test.Tasty.HUnit

import           Network.Syncthing.Internal.Error


createTestName :: Show err => err -> String
createTestName = ("Decode " ++) . show

createDescription :: Show err => err -> String -> String
createDescription errType errMsg =
    concat [show errMsg, " decodes to ", show errType]

testDecodeError :: SyncError -> BS.ByteString -> TestTree
testDecodeError errType errMsg =
    testCase (createTestName errType) $ 
        assertEqual (createDescription errType $ BS.unpack errMsg)
                    (Just errType)
                    (decodeError errMsg) 

testDecodeDeviceError :: DeviceError -> T.Text -> TestTree
testDecodeDeviceError errType errMsg =
    testCase (show errType) $ 
        assertEqual (createDescription errType $ T.unpack errMsg)
                    errType 
                    (decodeDeviceError errMsg)


errorUnits :: TestTree
errorUnits = testGroup "Unit Tests for error messages" $
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

