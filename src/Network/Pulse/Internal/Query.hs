
{-# LANGUAGE OverloadedStrings #-}

module Network.Pulse.Internal.Query
    ( query,
    ) where

import qualified Network.Wreq     as W
import Control.Monad.IO.Class     (liftIO)
import Control.Monad.Trans.Class  (lift)
import Control.Monad.Trans.Reader (ask)
import Data.Aeson                 (FromJSON, eitherDecode)
import Control.Lens               ((&), (^.), (.~), (^?))
import Data.Text.Encoding         (encodeUtf8)
import Data.Text                  (unpack)
import Control.Monad.Trans.Either (left, right)
import Control.Monad              (MonadPlus (mzero))

import Network.Pulse.Types


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
    
query :: FromJSON x => PulseRequest -> Pulse x
query request = do
    config <- lift ask
    let opts    = prepareOptions config (params request) W.defaults
    let server  = unpack $ config ^. pServer
    let url     = concat ["http://", server, path request]
    response <- liftIO $ W.getWith opts url 
    case (response ^? W.responseBody) of
        Nothing -> left RequestError
        Just bs -> 
            case (eitherDecode bs) of
                Left e   -> left $ ParseError e
                Right v  -> right v

