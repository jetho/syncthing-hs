
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.Pulse.Types 
    ( PulseConfig(..)
    , Param
    , HttpMethod
    , PulseRequest(..)
    , PulseError(..)
    , pServer
    , pApiKey
    , pAuth
    , pManager
    ) where

import Data.Typeable       (Typeable)
import Data.Text           as T
import Network.Wreq        (Auth)
import Network.HTTP.Client (Manager, ManagerSettings)
import Control.Lens        (makeLenses)


data PulseConfig = PulseConfig { 
      _pServer    :: T.Text
    , _pApiKey    :: Maybe T.Text
    , _pAuth      :: Maybe Auth
    , _pManager   :: Either ManagerSettings Manager
    }

$(makeLenses ''PulseConfig)

type Param = (T.Text, T.Text)

data HttpMethod = 
      Get 
    | Post
    deriving (Eq, Show)

data PulseRequest = PulseRequest { 
      method    :: HttpMethod
    , params    :: [Param]
    } deriving (Eq, Show)
    
data PulseError = InvalidApiKey
                deriving (Typeable, Eq, Show)
