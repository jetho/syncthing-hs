
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}


module Properties.JsonArbitrary where

import           Control.Applicative              (pure, (<$>), (<*>))
import           Data.Char                        (isSpace)
import           Data.Derive.Arbitrary
import           Data.DeriveTH
import qualified Data.Map                         as M
import qualified Data.Text                        as T
import           Test.QuickCheck.Instances
import           Test.Tasty.QuickCheck

import           Network.Syncthing.Internal


newtype NonEmptyText = NonEmptyText {getNonEmptyText :: T.Text}
                       deriving (Eq, Ord, Show, Read)

instance Arbitrary NonEmptyText where
    arbitrary = NonEmptyText . T.pack <$> listOf1 notSpace
      where notSpace = arbitrary `suchThat` (not . isSpace)
    shrink =   map NonEmptyText
             . filter (not . T.all isSpace)
             . shrink
             . getNonEmptyText

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

instance Arbitrary SystemMsg where
    arbitrary = oneof $ otherSystemMsg : knownMessages
      where
        knownMessages  = map pure [Restarting , ShuttingDown, ResettingDatabase]
        folderResetMsg = ResettingFolder <$> arbitrary
        otherSystemMsg = OtherSystemMsg <$> notReservedMsg
        notReservedMsg = arbitrary `suchThat` (not . isKnown)
        reservedMsgs   = [ "restarting", "shutting down", "resetting database"]
        isKnown msg    = or [ msg `elem` reservedMsgs 
                            , "resetting folder" `T.isPrefixOf` msg 
                            ]

instance Arbitrary Model where
    arbitrary =
        Model <$> arbitrary
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

instance Arbitrary GuiConfig where
    arbitrary =
        GuiConfig <$> arbitrary
                  <*> (fmap getNonEmptyText <$> arbitrary)
                  <*> genAddr
                  <*> arbitrary
                  <*> arbitrary
                  <*> arbitrary

instance Arbitrary DeviceConfig where
    arbitrary =
        DeviceConfig <$> arbitrary
                     <*> arbitrary
                     <*> (listOf $ oneof [pure Dynamic, Address <$> genAddr])
                     <*> arbitrary
                     <*> arbitrary
                     <*> arbitrary

instance Arbitrary OptionsConfig where
    arbitrary =
        OptionsConfig <$> (listOf genAddr)
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

instance Arbitrary DirTree where
    arbitrary = choose (0, 5) >>= dirTree
      where
        genFile = File <$> arbitrary <*> arbitrary

        dirTree :: Int -> Gen DirTree
        dirTree 0       = genFile
        dirTree n | n>0 = frequency [(2, genFile), (1, Dir <$> dirContents)]
          where
            dirContents = M.fromList <$> listOf dirEntry
            dirEntry    = (,) <$> entryName <*> dirTree (n `div` 2)
            entryName   = getNonEmptyText <$> arbitrary


-- | Let Template Haskell derive trivial instances.
concat <$> mapM (derive makeArbitrary)
                [ ''AddressType
                , ''FolderConfig
                , ''VersioningConfig
                , ''Config
                , ''Version
                , ''Ping
                , ''Completion
                , ''ModelState
                , ''Upgrade
                , ''Ignore
                , ''DBFile
                , ''FileInfo
                , ''Need
                , ''Sync
                , ''DeviceError
                , ''Error
                , ''Errors
                , ''System
                , ''UsageReport
                , ''Connections
                , ''DeviceInfo
                , ''FolderInfo
                , ''LastFile
                ]

