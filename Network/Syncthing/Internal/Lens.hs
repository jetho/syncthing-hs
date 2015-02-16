
{-# LANGUAGE TemplateHaskell #-}

module Network.Syncthing.Internal.Lens
    ( pServer
    , pApiKey
    , pAuth
    , pHttps
    , pManager
    ) where

import           Control.Lens            (makeLenses)
import           Network.Syncthing.Internal.Types (SyncConfig)

$(makeLenses ''SyncConfig)

