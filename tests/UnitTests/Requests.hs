
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module UnitTests.Requests
    ( requestUnits
    ) where

import           Control.Lens               ((&), (.~), (^.))
import           Control.Monad.Trans.Either (runEitherT)
import           Control.Monad.Trans.Reader (runReaderT)
import           Control.Monad.Trans.Writer (Writer, execWriter, tell)
import           Data.List                  (isPrefixOf)
import qualified Data.Text                  as T
import qualified Network.Wreq               as W
import           Test.Tasty
import           Test.Tasty.HUnit

import           Network.Syncthing
import qualified Network.Syncthing.Get      as Get
import           Network.Syncthing.Internal


-- | unary . binary function composition
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = fmap . fmap

data LoggedRequest = LoggedRequest {
      getUrl     :: String
    , getOptions :: W.Options
    }

type RequestLogger = Writer [LoggedRequest]

instance MonadSync RequestLogger where
    getMethod  o s   = tell [ LoggedRequest s o ] >> return ""
    postMethod o s p = tell [ LoggedRequest s o ] >> return ""

mockedSyncthing :: SyncConfig -> SyncM RequestLogger a -> RequestLogger (SyncResult a)
mockedSyncthing config action = 
    flip runReaderT config $ runEitherT $ runSyncthing action

extractRequest :: RequestLogger (SyncResult a) -> LoggedRequest
extractRequest = head . execWriter

execRequest :: SyncConfig -> SyncM RequestLogger a -> LoggedRequest
execRequest cfg action = extractRequest $ mockedSyncthing cfg action

getUrl' :: SyncConfig -> SyncM RequestLogger a -> String
getUrl' = getUrl .: execRequest

getOptions' :: SyncConfig -> SyncM RequestLogger a -> W.Options
getOptions' = getOptions .: execRequest

testUrl cfg action url =
    testCase url $
        getUrl' cfg action @?= "http://" ++ (T.unpack $ cfg ^. pServer) ++ url

configTests = 
    [ testCase "pHttps == False" $ 
        let cfg' = defaultConfig & pHttps .~ False in
        assertBool "url should start with http://" $ 
            "http://" `isPrefixOf` getUrl' cfg' Get.ping 

    , testCase "pHttps == True" $ 
        let cfg' = defaultConfig & pHttps .~ True in
        assertBool "url should start with https://" $ 
            "https://" `isPrefixOf` getUrl' cfg' Get.ping 

    , testCase "defaultConfig.pHttps == False" $ 
        assertBool "url should start with http://" $ 
            "http://" `isPrefixOf` getUrl' defaultConfig Get.ping 

    , testCase "pServer = 192.168.0.10:8080" $
        let cfg' = defaultConfig & pServer .~ "192.168.0.10:8080" in
        assertBool "url should start with http://192.168.0.10:8080" $ 
            "http://192.168.0.10:8080" `isPrefixOf` getUrl' cfg' Get.ping 
    
    , testCase "defaultConfig.pServer == 127.0.0.1:8080" $
        assertBool "url should start with http://127.0.0.1:8080" $
            "http://127.0.0.1:8080" `isPrefixOf` getUrl' defaultConfig Get.ping 
    ]

getTests = 
    [ testUrl defaultConfig Get.ping "/rest/ping" 
    , testUrl defaultConfig Get.version "/rest/version" 
    ]

requestUnits :: TestTree
requestUnits = testGroup "Requests" $ 
    concat [ getTests, configTests ]

