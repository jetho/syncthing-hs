
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module UnitTests.Requests
    ( requestUnits
    ) where

import           Control.Lens               ((&), (.~), (^.))
import           Control.Monad.Trans.Either (runEitherT)
import           Control.Monad.Trans.Reader (runReaderT)
import           Control.Monad.Trans.Writer (Writer, runWriter, tell)
import qualified Data.Text                  as T
import           Test.Tasty
import           Test.Tasty.HUnit

import           Network.Syncthing
import qualified Network.Syncthing.Get      as Get
import           Network.Syncthing.Internal


instance MonadSync (Writer String) where
    getMethod  o s   = tell s >> return ""
    postMethod o s p = tell s >> return ""

mockedSyncthing :: SyncConfig -> SyncM (Writer String) a -> Writer String (SyncResult a)
mockedSyncthing config action = 
    flip runReaderT config $ runEitherT $ runSyncthing action

extractRequest :: Writer String (SyncResult a) -> String
extractRequest = snd . runWriter

getUrl cfg action = extractRequest $ mockedSyncthing cfg action

testUrl cfg action url =
    testCase url $
        getUrl cfg action @?= "http://" ++ (T.unpack $ cfg ^. pServer) ++ url


requestUnits :: TestTree
requestUnits = testGroup "Requests"
    [ testUrl defaultConfig Get.ping "/rest/ping" ]

