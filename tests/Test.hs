
module Main where

import           Test.Tasty

import           Properties.ErrorProperties
import           Properties.JsonProperties
import           UnitTests.Errors


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [jsonProps, errorProps]

unitTests :: TestTree
unitTests = testGroup "Unit Tests" [errorUnits]

