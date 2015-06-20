
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE FlexibleContexts  #-}

module Properties.ErrorProperties
    ( errorProps
    ) where

import           Control.Applicative              ((<$>))
import           Control.Monad                    (ap)
import qualified Data.ByteString.Char8            as BS
import           Data.Char                        (chr)
import qualified Data.Text                        as T
import           Data.Text.Encoding               (decodeUtf8)
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Network.Syncthing.Internal


newtype FolderErrText = 
    FolderErrText { getFolderErrText :: BS.ByteString }
    deriving (Eq, Show)

newtype UnknownFolderName = 
    UnknownFolderName { getUnknownFolderName :: BS.ByteString }
    deriving (Eq, Show)

printableString :: Gen String
printableString = listOf1 . elements $ map chr [32 .. 126]

instance Arbitrary UnknownFolderName where
    arbitrary = UnknownFolderName . BS.pack <$> printableString

instance Arbitrary FolderErrText where
    arbitrary = FolderErrText . BS.pack . wrapText <$> printableString
      where wrapText t = "Folder " ++ t ++ " does not exist"

instance Arbitrary DeviceError where
    arbitrary = OtherDeviceError . T.pack <$> otherErrText
      where 
        otherErrText  = printableString `suchThat` flip notElem reservedMsgs
        reservedMsgs  = [ "device ID invalid: incorrect length"
                        , "check digit incorrect"
                        ]

encodeDeviceError :: DeviceError -> T.Text
encodeDeviceError (OtherDeviceError errMsg) = errMsg

prop_folderErr :: FolderErrText -> Bool
prop_folderErr = (Just NoSuchFolder ==) . parseError . getFolderErrText

prop_unknown :: UnknownFolderName -> Bool
prop_unknown fname = 
    Just (UnknownFolder (decodeUtf8 name)) == parseError msg
  where name = getUnknownFolderName fname
        msg  = "Unknown folder " `BS.append` name

prop_deviceErr :: DeviceError -> Bool
prop_deviceErr = ap (==) (decodeDeviceError . encodeDeviceError)

errorProps :: TestTree
errorProps = testGroup "Decoding Errors"
    [ testProperty "NoSuchFolder"  (prop_folderErr :: FolderErrText -> Bool)
    , testProperty "UnknownFolder" (prop_unknown :: UnknownFolderName -> Bool)
    , testProperty "DeviceError"   (prop_deviceErr :: DeviceError -> Bool)
    ]

