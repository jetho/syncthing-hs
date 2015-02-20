
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Network.Syncthing.Internal.Config
    ( Server 
    , SyncConfig(..)
    ) where

import qualified Data.Text                  as T
import           Network.HTTP.Client        (Manager, ManagerSettings)
import qualified Network.Wreq               as W


-- | Use the SERVER:PORT format for specifying servers.
type Server = T.Text

-- | The Syncthing configuration for specifying the Syncthing server,
-- authentication, the API Key etc.
data SyncConfig = SyncConfig {
      _pServer  :: Server
    , _pApiKey  :: Maybe T.Text
    , _pAuth    :: Maybe W.Auth
    , _pHttps   :: Bool
    , _pManager :: Either ManagerSettings Manager
    }

instance Show SyncConfig where
    show (SyncConfig {..}) =
        concat [ "SyncConfig { "
               , "pServer = ", show _pServer
               , ", pApiKey = ", show _pApiKey
               , ", pAuth = ", show _pAuth
               , ", pHttps = ", show _pHttps
               , ", pManager = ", case _pManager of
                      Left _  -> "Left _"
                      Right _ -> "Right _"
               , " }"
               ]

