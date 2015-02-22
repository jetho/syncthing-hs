
{-# LANGUAGE TemplateHaskell #-}


module SyncthingTest.Arbitrary where

import           Control.Applicative       ((<$>), (<*>))
import           Data.Derive.Arbitrary
import           Data.DeriveTH
import qualified Data.Text                 as T
import           Test.QuickCheck.Instances
import           Test.Tasty.QuickCheck

import           Network.Syncthing


genAddr :: Gen Addr
genAddr = (,) <$> host <*> port
  where 
    host      = T.pack <$> listOf1 hostChars
    hostChars = elements $ concat [['a'..'z'], ['A'.. 'Z'], ['0'..'9'], "./"]
    port      = fmap getNonNegative <$> arbitrary

instance Arbitrary CacheEntry where
    arbitrary = CacheEntry <$> genAddr <*> arbitrary

instance Arbitrary Connection where
    arbitrary = Connection <$> arbitrary 
                           <*> arbitrary
                           <*> arbitrary
                           <*> genAddr
                           <*> arbitrary

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
                ]

