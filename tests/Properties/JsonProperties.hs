
module Properties.JsonProperties
    ( jsonProps
    ) where

import           Data.Aeson                       
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Network.Syncthing.Internal.Error
import           Network.Syncthing.Types

import           Properties.JsonArbitrary
import           Properties.JsonInstances


type EitherDeviceErrorId = Either DeviceError Device


genProp name prop = testProperty testName prop
  where
    testName = name ++ " == decode . encode"

prop_json :: (Eq a, FromJSON a, ToJSON a) => a -> Bool
prop_json x = Just x == (decode . encode $ x)

jsonProps :: TestTree
jsonProps = testGroup "JSON Parsers"
    [ genProp "Ping"             (prop_json :: Ping -> Bool)
    , genProp "Version"          (prop_json :: Version -> Bool)
    , genProp "Completion"       (prop_json :: Completion -> Bool)
    , genProp "CacheEntry"       (prop_json :: CacheEntry -> Bool)
    , genProp "Connection"       (prop_json :: Connection -> Bool)
    , genProp "Model"            (prop_json :: Model -> Bool)
    , genProp "Upgrade"          (prop_json :: Upgrade -> Bool)
    , genProp "Ignore"           (prop_json :: Ignore -> Bool)
    , genProp "Progress"         (prop_json :: Progress -> Bool)
    , genProp "Need"             (prop_json :: Need -> Bool)
    , genProp "Sync"             (prop_json :: Sync -> Bool)
    , genProp "DeviceId"         (prop_json :: EitherDeviceErrorId -> Bool)
    , genProp "SystemMsg"        (prop_json :: SystemMsg -> Bool)
    , genProp "VersioningConfig" (prop_json :: VersioningConfig -> Bool)
    , genProp "FolderConfig"     (prop_json :: FolderConfig -> Bool)
    , genProp "GuiConfig"        (prop_json :: GuiConfig -> Bool)
    , genProp "OptionsConfig"    (prop_json :: OptionsConfig -> Bool)
    , genProp "DeviceConfig"     (prop_json :: DeviceConfig -> Bool)
    , genProp "Config"           (prop_json :: Config -> Bool)
    , genProp "Error"            (prop_json :: Error -> Bool)
    , genProp "Errors"           (prop_json :: Errors -> Bool)
    ]

