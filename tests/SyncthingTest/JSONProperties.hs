
module SyncthingTest.JSONProperties where

import           Data.Aeson                  (decode, encode)
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Network.Syncthing

import           SyncthingTest.Arbitrary
import           SyncthingTest.JSONInstances


prop_json x = Just x == (decode . encode $ x)

genTestName s = s ++ " == decode . encode"

jsonProps = testGroup "JSON parsing"
    [ testProperty (genTestName "Ping")    (prop_json :: Ping -> Bool)
    , testProperty (genTestName "Version") (prop_json :: Version -> Bool)
    ]

