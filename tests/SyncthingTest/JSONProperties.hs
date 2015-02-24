
module SyncthingTest.JSONProperties where

import           Data.Aeson                  (decode, encode)
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Network.Syncthing

import           SyncthingTest.Arbitrary
import           SyncthingTest.JSONInstances


prop_json x = Just x == (decode . encode $ x)

testName s = s ++ " == decode . encode"

jsonProps = testGroup "JSON parsing"
    [ testProperty (testName "Ping")       (prop_json :: Ping -> Bool)
    , testProperty (testName "Version")    (prop_json :: Version -> Bool)
    , testProperty (testName "Completion") (prop_json :: Completion -> Bool)
    , testProperty (testName "CacheEntry") (prop_json :: CacheEntry -> Bool)
    , testProperty (testName "Connection") (prop_json :: Connection -> Bool)
    , testProperty (testName "Model")      (prop_json :: Model -> Bool)
    ]

