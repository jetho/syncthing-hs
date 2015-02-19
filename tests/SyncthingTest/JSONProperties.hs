
module SyncthingTest.JSONProperties where

import           Data.Aeson                  (decode, encode)
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Network.Syncthing.Types
import           Network.Syncthing.Types.Ping

import           SyncthingTest.Arbitrary
import           SyncthingTest.JSONInstances


prop_json x = Just x == (decode . encode $ x)

jsonProps = testGroup "JSON parsing"
    [ testProperty "Ping == decode . encode" (prop_json :: Ping -> Bool)
    , testProperty "Version == decode . encode" (prop_json :: Version -> Bool)
    ]

