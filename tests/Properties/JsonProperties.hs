
module Properties.JsonProperties
    ( jsonProps
    ) where

import           Data.Aeson                       (decode, encode)
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Network.Syncthing.Internal.Error
import           Network.Syncthing.Types

import           Properties.JsonArbitrary
import           Properties.JsonInstances


test name prop = testProperty testName prop
  where
    testName = name ++ " == decode . encode"

prop_json x = Just x == (decode . encode $ x)

jsonProps = testGroup "JSON Parsers"
    [ test "Ping"             (prop_json :: Ping -> Bool)
    , test "Version"          (prop_json :: Version -> Bool)
    , test "Completion"       (prop_json :: Completion -> Bool)
    , test "CacheEntry"       (prop_json :: CacheEntry -> Bool)
    , test "Connection"       (prop_json :: Connection -> Bool)
    , test "Model"            (prop_json :: Model -> Bool)
    , test "Upgrade"          (prop_json :: Upgrade -> Bool)
    , test "Ignore"           (prop_json :: Ignore -> Bool)
    , test "Progress"         (prop_json :: Progress -> Bool)
    , test "Need"             (prop_json :: Need -> Bool)
    , test "Sync"             (prop_json :: Sync -> Bool)
    , test "DeviceId"         (prop_json :: (Either DeviceError Device) -> Bool)
    , test "SystemMsg"        (prop_json :: SystemMsg -> Bool)
    , test "VersioningConfig" (prop_json :: VersioningConfig -> Bool)
    , test "FolderConfig"     (prop_json :: FolderConfig -> Bool)
    , test "GuiConfig"        (prop_json :: GuiConfig -> Bool)
    , test "OptionsConfig"    (prop_json :: OptionsConfig -> Bool)
    , test "DeviceConfig"     (prop_json :: DeviceConfig -> Bool)
    , test "Config"           (prop_json :: Config -> Bool)
    , test "Error"            (prop_json :: Error -> Bool)
    , test "Errors"           (prop_json :: Errors -> Bool)
    ]

