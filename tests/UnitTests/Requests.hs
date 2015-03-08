
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module UnitTests.Requests
    ( requestUnits
    ) where

import           Control.Lens               ((&), (.~), (^.))
import           Control.Monad.Trans.Either (runEitherT)
import           Control.Monad.Trans.Reader (runReaderT)
import           Control.Monad.Trans.Writer (Writer, execWriter, tell)
import qualified Data.Text                  as T
import qualified Network.Wreq               as W
import           Test.Tasty
import           Test.Tasty.HUnit

import           Network.Syncthing
import qualified Network.Syncthing.Get      as Get
import           Network.Syncthing.Internal


type LoggedRequest = (String, W.Options)
type RequestLogger = Writer [LoggedRequest]

instance MonadSync RequestLogger where
    getMethod  o s   = tell [(s, o)] >> return ""
    postMethod o s p = tell [(s, o)] >> return ""

mockedSyncthing :: SyncConfig -> SyncM RequestLogger a -> RequestLogger (SyncResult a)
mockedSyncthing config action = 
    flip runReaderT config $ runEitherT $ runSyncthing action

extractRequest :: RequestLogger (SyncResult a) -> LoggedRequest
extractRequest = head . execWriter

getUrl cfg action = fst . extractRequest $ mockedSyncthing cfg action

testUrl cfg action url =
    testCase url $
        getUrl cfg action @?= "http://" ++ (T.unpack $ cfg ^. pServer) ++ url


requestUnits :: TestTree
requestUnits = testGroup "Requests"
    [ testUrl defaultConfig Get.ping "/rest/ping" 
    , testUrl defaultConfig Get.version "/rest/version" 
    ]

