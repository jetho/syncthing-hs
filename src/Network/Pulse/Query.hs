
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Network.Pulse.Query
    ( query
    ) where

import qualified Network.Wreq     as W
import Control.Monad.Trans.Class  (lift)
import Control.Monad.Trans.Reader (ask)
import Data.Aeson                 (FromJSON, eitherDecode, Value)
import Control.Lens               ((&), (^.), (.~), (^?))
import Data.Text.Encoding         (encodeUtf8)
import Data.Text                  (unpack)
import Control.Monad.Trans.Either (left, right)

import Network.Pulse.Types
import Network.Pulse.Lens


prepareOptions :: PulseConfig -> [Param] -> W.Options -> W.Options
prepareOptions cfg params' =   setManager (cfg ^. pManager)
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
    
query :: (MonadPulse (PulseM m), Monad m, FromJSON a) => PulseRequest -> PulseM m a
query request = do
    config     <- liftReader ask
    let opts    = prepareOptions config (params request) W.defaults
    let server  = unpack $ config ^. pServer
    let url     = concat ["http://", server, path request]
    respBody   <- case (method request) of
        Get          -> getMethod opts url 
        Post payload -> postMethod opts url payload
    liftEither $ case (eitherDecode respBody) of
        Left e   -> left $ ParseError e
        Right v  -> right v

