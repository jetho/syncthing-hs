
{-# LANGUAGE TemplateHaskell #-}


module SyncthingTest.Arbitrary where

import           Control.Applicative ((<$>))
import           Data.Derive.Arbitrary
import           Data.DeriveTH
import           Test.QuickCheck.Instances
import           Test.Tasty.QuickCheck

import           Network.Syncthing

concat <$> mapM (derive makeArbitrary) 
                [ ''AddressType
                , ''DeviceConfig
                , ''FolderConfig
                , ''VersioningConfig
                , ''GuiConfig
                , ''OptionsConfig
                , ''Config
                , ''Version
                , ''Ping
                , ''Completion
                , ''CacheEntry
                ]
