
module Properties.ErrorProperties
    ( errorProps
    ) where

import           Control.Applicative              ((<$>))
import           Data.ByteString.Lazy.Char8       (pack)
import           Data.Char                        (chr)
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Network.Syncthing.Internal.Error


newtype FolderText = FolderText { getFolderText :: String }
                     deriving (Eq, Show)

printableChars = map chr [32 .. 126]

instance Arbitrary FolderText where
    arbitrary = FolderText . wrapText <$> (listOf1 $ elements printableChars)
      where wrapText t = "Folder " ++ t ++ " does not exist"

prop_error = (Just NoSuchFolder ==) . decodeError . pack . getFolderText

errorProps = testGroup "Decoding Errors"
    [ testProperty "decode NoSuchFolder" (prop_error :: FolderText -> Bool)
    ]

