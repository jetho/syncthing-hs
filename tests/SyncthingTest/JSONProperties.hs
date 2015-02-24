
module SyncthingTest.JSONProperties where

import           Data.Aeson                  (decode, encode)
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Network.Syncthing

import           SyncthingTest.Arbitrary
import           SyncthingTest.JSONInstances


jsonTest name prop = testProperty testName prop
  where
    testName = name ++ " == decode . encode"

prop_json x = Just x == (decode . encode $ x)

jsonProps = testGroup "Parsing JSON"
    [ jsonTest "Ping"       (prop_json :: Ping -> Bool)
    , jsonTest "Version"    (prop_json :: Version -> Bool)
    , jsonTest "Completion" (prop_json :: Completion -> Bool)
    , jsonTest "CacheEntry" (prop_json :: CacheEntry -> Bool)
    , jsonTest "Connection" (prop_json :: Connection -> Bool)
    , jsonTest "Model"      (prop_json :: Model -> Bool)
    , jsonTest "Upgrade"    (prop_json :: Upgrade -> Bool)
    ]

