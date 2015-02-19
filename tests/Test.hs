
{-# LANGUAGE TemplateHaskell #-}


import           Control.Applicative       ((<$>))
import           Data.Aeson                (decode, encode)
import           Data.Derive.Arbitrary
import           Data.DeriveTH
import           Data.String               (fromString)
import           Data.Text                 (Text)
import           Test.QuickCheck.Instances
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Network.Syncthing.Types


$(derive makeArbitrary ''AddressType)
$(derive makeArbitrary ''DeviceConfig)
$(derive makeArbitrary ''FolderConfig)
$(derive makeArbitrary ''VersioningConfig)
$(derive makeArbitrary ''GuiConfig)
$(derive makeArbitrary ''OptionsConfig)
$(derive makeArbitrary ''Config)


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties]

properties :: TestTree
properties = testGroup "Properties" [jsonProps]

prop_json x = Just x == (decode . encode $ x)

jsonProps = testGroup "JSON parsing"
    [ testProperty "config == decode . encode" (prop_json :: Config -> Bool)
    ]
