
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module UnitTests.Requests
    ( requestUnits
    ) where

import           Control.Applicative        ((<$>))
import           Control.Lens               ((&), (.~), (?~), (^.))
import           Control.Monad.Trans.Writer (Writer, execWriter, tell)
import           Data.Aeson                 (ToJSON, Value, toJSON)
import           Data.List                  (isPrefixOf, sort)
import qualified Data.Map                   as M
import qualified Data.Text                  as T
import           Data.Text.Encoding         (encodeUtf8)
import qualified Network.Wreq               as W
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

import           Network.Syncthing
import qualified Network.Syncthing.Get      as Get
import           Network.Syncthing.Internal
import qualified Network.Syncthing.Post     as Post

import           Properties.JsonArbitrary
import           Properties.JsonInstances



-------------- Test Infrastructure --------------

type Endpoint = String
type Url      = String
type Params   = [Param]

data RequestType = GET | POST
                 deriving (Eq, Show)

data LoggedRequest = LoggedRequest {
      reqType    :: RequestType
    , reqUrl     :: String
    , reqOptions :: W.Options
    , reqPayload :: Maybe Value
    }

type RequestLogger  = Writer [LoggedRequest]
type LogAction      = SyncM RequestLogger
type LogResult a    = RequestLogger (SyncResult a)

instance MonadSync RequestLogger where
    getMethod  o s   = tell [ LoggedRequest GET  s o Nothing  ] >> return ""
    postMethod o s p = tell [ LoggedRequest POST s o (Just p) ] >> return ""

mockedSyncthing :: SyncConfig -> LogAction a -> LogResult a
mockedSyncthing = runSyncM

extractRequest :: LogResult a -> LoggedRequest
extractRequest = head . execWriter

execRequest :: SyncConfig -> LogAction a -> LoggedRequest
execRequest cfg action = extractRequest $ mockedSyncthing cfg action

withConfigRequest :: SyncConfig -> LogAction a -> (LoggedRequest -> b) -> b
withConfigRequest cfg action f = f $ execRequest cfg action

withRequest :: LogAction a -> (LoggedRequest -> b) -> b
withRequest = withConfigRequest defaultConfig



-------------- Helper Functions --------------

noPayload :: ()
noPayload = ()

noParams :: Params
noParams = []

createIgnoresMap :: [T.Text] -> M.Map T.Text [T.Text] 
createIgnoresMap = M.singleton "ignore" 

serverString :: SyncConfig -> String
serverString = T.unpack . (^. pServer)

assertReqType :: RequestType -> RequestType -> Assertion
assertReqType rType reqType =
    assertEqual (show rType ++ " Request is executed") rType reqType 

assertUrl :: SyncConfig -> Endpoint -> Url -> Assertion
assertUrl cfg endpoint reqUrl =
    let url = "http://" ++ serverString cfg ++ endpoint
    in  assertEqual "Request url is correct" url reqUrl

assertParams :: Params -> W.Options -> Assertion
assertParams params reqOptions =
    let reqParams = reqOptions ^. W.params
    in  assertEqual "Params set correctly" (sort params) (sort reqParams)

assertPayload :: ToJSON p => p -> Maybe Value -> Assertion 
assertPayload payload reqPayload =
    let payload' = Just . toJSON $ payload
    in  assertEqual "Payload injected correctly" payload' reqPayload

assertGet :: LoggedRequest -> Endpoint -> Params -> Assertion
assertGet LoggedRequest{..} endpoint params = do
    assertReqType GET reqType
    assertUrl defaultConfig endpoint reqUrl
    assertParams params reqOptions

assertPost :: ToJSON p => LoggedRequest -> Endpoint -> Params -> p -> Assertion
assertPost LoggedRequest{..} endpoint params payload = do
    assertReqType POST reqType
    assertUrl defaultConfig endpoint reqUrl
    assertParams params reqOptions
    assertPayload payload reqPayload



-------------- Basic Tests --------------

basicReq rType endpoint action  = withRequest action $
    \LoggedRequest{..} -> do
        assertReqType rType reqType 
        assertUrl defaultConfig endpoint reqUrl
        assertEqual "Accept header contains 'application/json'"
            ["application/json"] (reqOptions ^. W.header "Accept")

changeServer =
    let server = "192.168.0.10:8080"
        cfg    = defaultConfig & pServer .~ server
    in
    withConfigRequest cfg Get.ping $
        \LoggedRequest{..} -> assertUrl cfg "/rest/system/ping" reqUrl

setApiKey =
    let apiKey = "123456789XYZ"
        cfg    = defaultConfig & pApiKey ?~ apiKey
    in
    withConfigRequest cfg Get.ping $
        \LoggedRequest{..} -> 
            assertEqual "ApiKey is set as X-API-KEY header"
                [encodeUtf8 apiKey] (reqOptions ^. W.header "X-API-KEY")

enableAuth =
    let auth = W.basicAuth "user" "pass"
        cfg  = defaultConfig & pAuth ?~ auth
    in
    withConfigRequest cfg Get.ping $
        \LoggedRequest{..} -> 
            assertEqual "Auth enabled" (Just auth) (reqOptions ^. W.auth)

enableHttps =
    let cfg = defaultConfig & pHttps .~ True
    in
    withConfigRequest cfg Get.ping $
        \LoggedRequest{..} -> 
            assertBool "HTTPS protocol is used" $ "https://" `isPrefixOf` reqUrl

disableHttps =
    let cfg = defaultConfig & pHttps .~ False
    in
    withConfigRequest cfg Get.ping $
        \LoggedRequest{..} -> 
            assertBool "HTTP protocol is used" $ "http://" `isPrefixOf` reqUrl



-------------- GET and POST Requests --------------

testGet :: Endpoint -> Params -> LogAction a -> TestTree
testGet endpoint params action = withRequest action $
    \loggedRequest ->
        testCase ("Test GET " ++ endpoint) $ 
            assertGet loggedRequest endpoint params            

testPost :: ToJSON p => Endpoint -> Params -> p -> LogAction a -> TestTree
testPost endpoint params payload action = withRequest action $
    \loggedRequest ->
        testCase ("Test POST " ++ endpoint) $ 
            assertPost loggedRequest endpoint params payload

-- | Post.sendConfig requires special treatment. Use Quickcheck to generate
-- sample data for the payload.
testPostConfig :: Endpoint -> Params -> TestTree
testPostConfig endpoint params = 
    testCase ("Test POST " ++ endpoint) $ do
        configSample <- head <$> sample' (arbitrary :: Gen Config)
        withRequest (Post.config configSample) $ 
            \loggedRequest -> 
                assertPost loggedRequest endpoint params configSample



-------------- Test Suite --------------

requestUnits :: TestTree
requestUnits = testGroup "Unit Tests for Requests" 
    [ testGroup "Basic Tests"
        [ testCase "GET Request"  $ basicReq GET  "/rest/system/ping" Get.ping
        , testCase "POST Request" $ basicReq POST "/rest/system/ping" Post.ping
        , testCase "changeServer"           changeServer
        , testCase "set ApiKey"             setApiKey
        , testCase "enable Authentication"  enableAuth
        , testCase "enable HTTPS usage"     enableHttps
        , testCase "disable HTTPS usage"    disableHttps
        ]
    , testGroup "GET Requests"
        [ testGet "/rest/system/ping"           noParams Get.ping
        , testGet "/rest/db/completion"
                  [("folder","default"), ("device", "device1")] 
                  (Get.completion "device1" "default")
        , testGet "/rest/system/config"         noParams Get.config
        , testGet "/rest/system/connections"    noParams Get.connections
        , testGet "/rest/svc/deviceid" 
                  [("id", "device1")] 
                  (Get.deviceId "device1")
        , testGet "/rest/system/discovery"      noParams Get.discovery
        , testGet "/rest/system/error"          noParams Get.errors
        , testGet "/rest/db/ignores" 
                  [("folder", "default")] 
                  (Get.ignores "default")
        , testGet "/rest/db/status" 
                  [("folder", "default")] 
                  (Get.dbStatus "default")
        , testGet "/rest/db/need"  
                  [("folder", "default")] 
                  (Get.need "default")
        , testGet "/rest/system/config/insync"  noParams Get.insync
        , testGet "/rest/svc/report"            noParams Get.report
        , testGet "/rest/system/status"         noParams Get.sysStatus
        , testGet "/rest/db/browse"        
                  [("folder", "default")]
                  (Get.browse "default" Nothing Nothing)
        , testGet "/rest/db/browse"        
                  [("folder", "default"), ("prefix", "foo/bar")]
                  (Get.browse "default" (Just "foo/bar") Nothing)
        , testGet "/rest/db/browse"        
                  [("folder", "default"), ("levels", "2")]
                  (Get.browse "default" Nothing (Just 2))
        , testGet "/rest/db/browse"        
                  [("folder", "default"),("prefix", "foo/bar"),("levels", "2")]
                  (Get.browse "default" (Just "foo/bar") (Just 2))
        , testGet "/rest/db/file"        
                  [("folder", "default"), ("file", "foo/bar")]
                  (Get.file "default" "foo/bar")
        , testGet "/rest/system/upgrade"        noParams Get.upgrade
        , testGet "/rest/system/version"        noParams Get.version
        , testGet "/rest/svc/lang"              noParams Get.lang
        , testGet "/rest/stats/device"          noParams Get.devices
        , testGet "/rest/stats/folder"          noParams Get.folders
        ]
    , testGroup "POST Requests"
        [ testPost "/rest/system/ping"          noParams noPayload Post.ping
        , testPost "/rest/db/prio"
                   [("folder", "default"), ("file", "foo/bar")] 
                   noPayload
                   (Post.prio "default" "foo/bar")
        , testPost "/rest/system/discovery"
                   [("device", "device1"), ("addr", "192.168.0.10:8080")] 
                   noPayload
                   (Post.discovery "device1" "192.168.0.10:8080")
        , testPost "/rest/system/error" 
                   noParams 
                   (T.pack "Error 1") 
                   (Post.sendError "Error 1")
        , testPost "/rest/system/error/clear" 
                   noParams noPayload Post.clearErrors
        , testPost "/rest/db/scan" 
                   [("folder", "default")] 
                   noPayload 
                   (Post.scan "default" Nothing Nothing)
        , testPost "/rest/db/scan"
                   [("folder", "default"), ("sub", "foo/bar")] 
                   noPayload 
                   (Post.scan "default" (Just "foo/bar") Nothing)
        , testPost "/rest/db/scan"
                   [("folder", "default"), ("next", "10")] 
                   noPayload 
                   (Post.scan "default" Nothing (Just 10))
        , testPost "/rest/db/scan"
                   [("folder", "default"), ("sub", "foo/bar"), ("next", "10")] 
                   noPayload 
                   (Post.scan "default" (Just "foo/bar") (Just 10))
        , testPost "/rest/system/restart"  noParams noPayload Post.restart
        , testPost "/rest/system/shutdown" noParams noPayload Post.shutdown
        , testPost "/rest/system/reset" noParams noPayload (Post.reset Nothing)
        , testPost "/rest/system/reset" 
                   [("folder", "foo")] 
                   noPayload 
                   (Post.reset $ Just "foo")
        , testPost "/rest/system/upgrade"  noParams noPayload Post.upgrade
        , testPost "/rest/db/ignores" 
                   [("folder", "default")] 
                   (createIgnoresMap ["file1", "file2", "foo/bar"])
                   (Post.ignores "default" ["file1", "file2", "foo/bar"])
        , testPostConfig "/rest/system/config" noParams
        ]
    ]

