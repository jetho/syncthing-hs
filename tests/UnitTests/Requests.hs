
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module UnitTests.Requests
    ( requestUnits
    ) where

import           Control.Lens               ((&), (.~), (?~), (^.))
import           Control.Monad.Trans.Either (runEitherT)
import           Control.Monad.Trans.Reader (runReaderT)
import           Control.Monad.Trans.Writer (Writer, execWriter, tell)
import           Data.Aeson                 (ToJSON, Value, toJSON)
import           Data.List                  (isPrefixOf, sort)
import qualified Data.Map                   as M
import qualified Data.Text                  as T
import           Data.Text.Encoding         (encodeUtf8)
import qualified Network.Wreq               as W
import           Test.Tasty
import           Test.Tasty.HUnit

import           Network.Syncthing
import qualified Network.Syncthing.Get      as Get
import           Network.Syncthing.Internal
import qualified Network.Syncthing.Post     as Post



-------------- Test Infrastructure --------------

data LoggedRequest = LoggedRequest {
      reqUrl     :: String
    , reqOptions :: W.Options
    , reqPayload :: Maybe Value
    }

type RequestLogger  = Writer [LoggedRequest]
type LogAction      = SyncM RequestLogger
type LoggerResult a = RequestLogger (SyncResult a)

instance MonadSync RequestLogger where
    getMethod  o s   = tell [ LoggedRequest s o Nothing  ] >> return ""
    postMethod o s p = tell [ LoggedRequest s o (Just p) ] >> return ""

mockedSyncthing :: SyncConfig -> LogAction a -> LoggerResult a
mockedSyncthing config action =
    flip runReaderT config $ runEitherT $ runSyncthing action

extractRequest :: LoggerResult a -> LoggedRequest
extractRequest = head . execWriter

execRequest :: SyncConfig -> LogAction a -> LoggedRequest
execRequest cfg action = extractRequest $ mockedSyncthing cfg action

withConfigRequest :: SyncConfig -> LogAction a -> (LoggedRequest -> b) -> b
withConfigRequest cfg action f = f $ execRequest cfg action

withRequest :: LogAction a -> (LoggedRequest -> b) -> b
withRequest = withConfigRequest defaultConfig

serverString :: SyncConfig -> String
serverString = T.unpack . (^. pServer)



-------------- Basic Tests --------------

basicGetRequest = withRequest Get.ping $
    \LoggedRequest {..} -> do
        assertBool "correct url is generated" $
            ("http://" ++ serverString defaultConfig) `isPrefixOf` reqUrl
        assertEqual "Accept header contains application/json"
            ["application/json"] (reqOptions ^. W.header "Accept")

basicPostRequest = withRequest Post.ping $
    \LoggedRequest {..} -> do
        assertBool "correct url is generated" $
            ("http://" ++ serverString defaultConfig) `isPrefixOf` reqUrl
        assertEqual "Accept header contains application/json"
            ["application/json"] (reqOptions ^. W.header "Accept")

changeServer =
    let server = "192.168.0.10:8080"
        cfg    = defaultConfig & pServer .~ server
    in
    withConfigRequest cfg Get.ping $
        \LoggedRequest {..} -> 
            assertBool "Url reflects server modification" $
                ("http://" ++ serverString cfg) `isPrefixOf` reqUrl

setApiKey =
    let apiKey = "123456789XYZ"
        cfg    = defaultConfig & pApiKey ?~ apiKey
    in
    withConfigRequest cfg Get.ping $
        \LoggedRequest {..} -> 
            assertEqual "ApiKey is set as X-API-KEY header"
                [encodeUtf8 apiKey] (reqOptions ^. W.header "X-API-KEY")

enableAuth =
    let auth = W.basicAuth "user" "pass"
        cfg  = defaultConfig & pAuth ?~ auth
    in
    withConfigRequest cfg Get.ping $
        \LoggedRequest {..} -> 
            assertEqual "Authentication enabled"
                (Just auth) (reqOptions ^. W.auth)

enableHttps =
    let cfg = defaultConfig & pHttps .~ True
    in
    withConfigRequest cfg Get.ping $
        \LoggedRequest {..} -> 
            assertBool "HTTPS protocol is used" $ "https://" `isPrefixOf` reqUrl

disableHttps =
    let cfg = defaultConfig & pHttps .~ False
    in
    withConfigRequest cfg Get.ping $
        \LoggedRequest {..} -> 
            assertBool "HTTP protocol is used" $ "http://" `isPrefixOf` reqUrl



-------------- GET and POST Requests --------------

testGet :: String -> [Param] -> LogAction a -> TestTree
testGet endpoint params action = withRequest action $
    \LoggedRequest {..} ->
        testCase ("Test GET " ++ endpoint) $ do
            let url = "http://" ++ serverString defaultConfig ++ endpoint
            assertEqual "Request url is correct" url reqUrl

            let reqParams = reqOptions ^. W.params
            assertEqual "Params set correctly" (sort params) (sort reqParams)


testPost :: ToJSON v => String -> [Param] -> v -> LogAction a -> TestTree
testPost endpoint params val action = withRequest action $
    \LoggedRequest {..} ->
        testCase ("Test POST " ++ endpoint) $ do
            let url = "http://" ++ serverString defaultConfig ++ endpoint
            assertEqual "Request url is correct" url reqUrl

            let reqParams = reqOptions ^. W.params
            assertEqual "Params set correctly" (sort params) (sort reqParams)

            let payload = Just . toJSON $ val
            assertEqual "Payload injected correctly" payload reqPayload



-------------- Test Suite --------------

requestUnits :: TestTree
requestUnits = testGroup "Unit Tests for Requests" $
    [ testGroup "Basic Tests"
        [ testCase "basic GET Request"      basicGetRequest
        , testCase "basic POST Request"     basicPostRequest
        , testCase "changeServer"           changeServer
        , testCase "set ApiKey"             setApiKey
        , testCase "enable Authentication"  enableAuth
        , testCase "enable HTTPS usage"     enableHttps
        , testCase "disable HTTPS usage"    disableHttps
        ]
    , testGroup "GET Requests"
        [ testGet "/rest/ping" [] Get.ping
        , testGet "/rest/completion"
                  [("folder","default"), ("device", "device1")] $
                  Get.completion "device1" "default"
        , testGet "/rest/config" [] Get.config
        , testGet "/rest/connections" [] Get.connections
        , testGet "/rest/deviceid" [("id", "device1")] $ Get.deviceId "device1"
        , testGet "/rest/discovery" [] Get.discovery
        , testGet "/rest/errors" [] Get.errors
        , testGet "/rest/ignores" [("folder", "default")] $
                  Get.ignores "default"
        , testGet "/rest/model" [("folder", "default")] $ Get.model "default"
        , testGet "/rest/need" [("folder", "default")] $ Get.need "default"
        , testGet "/rest/config/sync" [] Get.sync
        , testGet "/rest/system" [] Get.system
        , testGet "/rest/upgrade" [] Get.upgrade
        , testGet "/rest/version" [] Get.version
        ]
    , testGroup "POST Requests"
        [ testPost "/rest/ping" [] () Post.ping
        , testPost "/rest/bump"
                   [("folder", "default"), ("file", "foo/bar")] () $
                   Post.bump "default" "foo/bar"
        , testPost "/rest/discovery/hint"
                   [("device", "device1"), ("addr", "192.168.0.10:8080")] () $
                   Post.hint "device1" "192.168.0.10:8080"
        , testPost "/rest/error" [] (T.pack "Error 1") $
                   Post.sendError "Error 1"
        , testPost "/rest/error/clear" [] () Post.clearErrors
        , testPost "/rest/scan" [("folder", "default")] () $
                   Post.scanFolder "default" Nothing
        , testPost "/rest/scan"
                   [("folder", "default"), ("sub", "foo/bar")] () $
                   Post.scanFolder "default" (Just "foo/bar")
        , testPost "/rest/restart" [] () Post.restart
        , testPost "/rest/shutdown" [] () Post.shutdown
        , testPost "/rest/reset" [] () Post.reset
        , testPost "/rest/upgrade" [] () Post.upgrade
        , let ignores = ["file1", "file2", "foo/bar"]
              ignMap  = M.singleton "ignore" ignores :: M.Map T.Text [T.Text]
          in testPost "/rest/ignores" [("folder", "default")] ignMap $
                      Post.sendIgnores "default" ignores
        ]
    ]

