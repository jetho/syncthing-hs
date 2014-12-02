
{-# LANGUAGE TemplateHaskell #-}

module Network.Pulse.Lens
    ( pServer
    , pApiKey
    , pAuth
    , pHttps
    , pManager
    ) where

import           Control.Lens        (makeLenses)
import           Network.Pulse.Types (PulseConfig)

$(makeLenses ''PulseConfig)
