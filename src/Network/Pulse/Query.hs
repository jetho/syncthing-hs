
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Pulse.Query
    ( query
    ) where

import           Control.Lens               ((&), (.~), (^.))
import           Control.Monad.Trans.Reader (ask)
import           Data.Aeson                 (FromJSON, eitherDecode)
import           Data.Text                  (unpack)
import           Data.Text.Encoding         (encodeUtf8)
import qualified Network.Wreq               as W

import           Network.Pulse.Lens
import           Network.Pulse.Types


prepareOptions :: PulseConfig -> [Param] -> W.Options -> W.Options
prepareOptions cfg params' =
      setManager (cfg ^. pManager)
    . setApiKey  (cfg ^. pApiKey)
    . setAuth    (cfg ^. pAuth)
    . setParams
    . setJsonHeader
    where
        setManager mgr          = (& W.manager .~ mgr)
        setAuth authInfo        = (& W.auth .~ authInfo)
        setJsonHeader           = (& W.header "Accept" .~ ["application/json"])
        setParams               = (& W.params .~ params')
        setApiKey (Just apiKey) = (& W.header "X-API-Key" .~ [encodeUtf8 apiKey])
        setApiKey Nothing       = id

query :: (MonadPulse m, FromJSON a) => PulseRequest -> PulseM m a
query request = do
    config     <- liftReader ask
    let opts    = prepareOptions config (params request) W.defaults
    let server  = unpack $ config ^. pServer
    let proto   = if (config ^. pHttps) then "https://" else "http://"
    let url     = concat [proto, server, path request]
    respBody   <- liftInner $
        case method request of
            Get          -> getMethod opts url
            Post payload -> postMethod opts url payload
    either (liftLeft . ParseError)
           liftRight
           (eitherDecode respBody)

