
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Syncthing.Internal.Request
    ( Param
    , HttpMethod(..)
    , SyncRequest(..)
    , query
    , queryMaybe
    , send
    , get
    , post
    , getRequest
    , postRequest
    ) where

import           Control.Lens                      ((&), (.~), (^.))
import           Control.Monad                     ((<=<), (>=>))
import           Control.Monad.Trans.Reader        (ask)
import           Data.Aeson                        
import           Data.ByteString.Lazy              (ByteString)
import qualified Data.Text                         as T
import           Data.Text.Encoding                (encodeUtf8)
import qualified Network.Wreq                      as W

import           Network.Syncthing.Internal.Config
import           Network.Syncthing.Internal.Error
import           Network.Syncthing.Internal.Monad


type Param = (T.Text, T.Text)

data HttpMethod =
      Get
    | Post Value
    deriving (Eq, Show)

data SyncRequest = SyncRequest {
      path   :: String
    , method :: HttpMethod
    , params :: [Param]
    } deriving (Eq, Show)

query :: (MonadSync m, FromJSON a) => SyncRequest -> SyncM m a
query = either (liftLeft . ParseError) liftRight . eitherDecode <=< request

queryMaybe :: (MonadSync m, FromJSON a) => SyncRequest -> SyncM m (Maybe a)
queryMaybe = request >=> \case
    "" -> liftRight Nothing
    bs -> liftRight $ decode bs

send :: MonadSync m => SyncRequest -> SyncM m ()
send = const (liftRight ()) <=< request

request :: MonadSync m => SyncRequest -> SyncM m ByteString
request req = do
    config     <- liftReader ask
    let opts    = prepareOptions config (params req) W.defaults
    let server' = T.unpack $ config ^. pServer
    let proto   = if (config ^. pHttps) then "https://" else "http://"
    let url     = concat [proto, server', path req]
    liftInner $
        case method req of
            Get          -> getMethod opts url
            Post payload -> postMethod opts url payload

prepareOptions :: SyncConfig -> [Param] -> W.Options -> W.Options
prepareOptions cfg params' =
      setManager (cfg ^. pManager)
    . setApiKey  (cfg ^. pApiKey)
    . setAuth    (cfg ^. pAuth)
    . setParams
    . setJsonHeader
  where
    setManager mgr           = (& W.manager .~ mgr)
    setAuth authInfo         = (& W.auth .~ authInfo)
    setJsonHeader            = (& W.header "Accept" .~ ["application/json"])
    setParams                = (& W.params .~ params')
    setApiKey (Just apiKey') = (& W.header "X-API-Key" .~ [encodeUtf8 apiKey'])
    setApiKey Nothing        = id

get :: HttpMethod
get = Get

post :: ToJSON a => a -> HttpMethod
post = Post . toJSON

getRequest :: SyncRequest
getRequest = SyncRequest {
      path   = "/rest/ping"
    , method = get
    , params = []
    }

postRequest :: SyncRequest
postRequest = SyncRequest {
      path   = "/rest/ping"
    , method = post ()
    , params = []
    }

