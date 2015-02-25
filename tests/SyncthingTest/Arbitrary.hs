
{-# LANGUAGE TemplateHaskell #-}


module SyncthingTest.Arbitrary where

import           Control.Applicative       ((<$>), (<*>))
import           Data.Char                 (isSpace)
import           Data.Derive.Arbitrary
import           Data.DeriveTH
import qualified Data.Text                 as T
import           Test.QuickCheck.Instances
import           Test.Tasty.QuickCheck

import           Network.Syncthing


newtype NonEmptyText = NonEmptyText {getNonEmptyText :: T.Text}
                       deriving (Eq, Ord, Show, Read)

instance Arbitrary NonEmptyText where
    arbitrary = NonEmptyText . T.pack <$> listOf1 (arbitrary `suchThat` (not . isSpace))
    shrink (NonEmptyText txt) = map NonEmptyText . filter (not . T.all isSpace) $ shrink txt

genAddr :: Gen Addr
genAddr = (,) <$> host <*> port
  where
    host      = T.pack <$> listOf1 hostChars
    hostChars = elements $ concat [['a'..'z'], ['A'.. 'Z'], ['0'..'9'], "."]
    port      = fmap getNonNegative <$> arbitrary

instance Arbitrary CacheEntry where
    arbitrary = CacheEntry <$> genAddr <*> arbitrary

instance Arbitrary Connection where
    arbitrary = Connection <$> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> genAddr
                           <*> arbitrary

instance Arbitrary Model where
    arbitrary = Model <$> arbitrary
                      <*> arbitrary
                      <*> arbitrary
                      <*> arbitrary
                      <*> arbitrary
                      <*> arbitrary
                      <*> arbitrary
                      <*> arbitrary
                      <*> arbitrary
                      <*> arbitrary
                      <*> arbitrary
                      <*> arbitrary
                      <*> (fmap getNonEmptyText <$> arbitrary)
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
                , ''ModelState
                , ''Upgrade
                , ''Ignore
                , ''Progress    
                , ''Need
                , ''Sync
                , ''DeviceError
                ]

