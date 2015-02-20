
{-# LANGUAGE TemplateHaskell #-}


module SyncthingTest.Arbitrary where

import           Data.Derive.Arbitrary
import           Data.DeriveTH
import           Test.QuickCheck.Instances
import           Test.Tasty.QuickCheck

import           Network.Syncthing


$(derive makeArbitrary ''AddressType)
$(derive makeArbitrary ''DeviceConfig)
$(derive makeArbitrary ''FolderConfig)
$(derive makeArbitrary ''VersioningConfig)
$(derive makeArbitrary ''GuiConfig)
$(derive makeArbitrary ''OptionsConfig)
$(derive makeArbitrary ''Config)
$(derive makeArbitrary ''Version)
$(derive makeArbitrary ''Ping)
$(derive makeArbitrary ''Completion)

