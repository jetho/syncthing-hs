
module Main where

import           Test.Tasty
import           Test.Tasty.QuickCheck

import           SyncthingTest.ErrorProperties
import           SyncthingTest.JsonProperties


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties]

properties :: TestTree
properties = testGroup "Properties" [jsonProps, errorProps]

