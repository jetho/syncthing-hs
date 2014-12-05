
{-# LANGUAGE TemplateHaskell #-}

module Network.Syncthing.Lens
    ( pServer
    , pApiKey
    , pAuth
    , pHttps
    , pManager
    ) where

import           Control.Lens            (makeLenses)
import           Network.Syncthing.Types (SyncthingConfig)

$(makeLenses ''SyncthingConfig)
