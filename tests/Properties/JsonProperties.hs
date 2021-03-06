
module Properties.JsonProperties
    ( jsonProps
    ) where

import           Data.Aeson                       
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Network.Syncthing.Internal

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
    , genProp "Connections"      (prop_json :: Connections -> Bool)
    , genProp "Model"            (prop_json :: Model -> Bool)
    , genProp "Upgrade"          (prop_json :: Upgrade -> Bool)
    , genProp "Ignore"           (prop_json :: Ignore -> Bool)
    , genProp "FileInfo"         (prop_json :: FileInfo -> Bool)
    , genProp "DBFile"           (prop_json :: DBFile -> Bool)
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
    , genProp "System"           (prop_json :: System -> Bool)
    , genProp "DirTree"          (prop_json :: DirTree -> Bool)
    , genProp "UsageReport"      (prop_json :: UsageReport -> Bool)
    , genProp "DeviceInfo"       (prop_json :: DeviceInfo -> Bool)
    , genProp "FolderInfo"       (prop_json :: FolderInfo -> Bool)
    , genProp "LastFile"         (prop_json :: LastFile -> Bool)
    ]

