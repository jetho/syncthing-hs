
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           SyncthingTest.JSONProperties


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties]

properties :: TestTree
properties = testGroup "Properties" [jsonProps]

