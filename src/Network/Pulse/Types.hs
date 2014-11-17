
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

module Network.Pulse.Types 
    ( Pulse
    , PulseConfig(..)
    , Param
    , HttpMethod(..)
    , PulseRequest(..)
    , PulseError(..)
    ) where

import Data.Typeable              (Typeable)
import qualified Data.Text        as T
import Network.Wreq               (Auth)
import Network.HTTP.Client        (Manager, ManagerSettings)
import Control.Monad.Trans.Either (EitherT)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Aeson                 (Value)


data PulseConfig = PulseConfig { 
      _pServer    :: T.Text
    , _pApiKey    :: Maybe T.Text
    , _pAuth      :: Maybe Auth
    , _pManager   :: Either ManagerSettings Manager
    }

type Pulse a = EitherT PulseError (ReaderT PulseConfig IO) a

type Param = (T.Text, T.Text)

data HttpMethod = 
      Get 
    | Post Value
    deriving (Eq, Show)

data PulseRequest = PulseRequest { 
      path      :: String
    , method    :: HttpMethod
    , params    :: [Param]
    } deriving (Eq, Show)
    
data PulseError = 
      InvalidApiKey 
    | ParseError String 
    | NotFound 
    | RequestError 
    | Unauthorized
    deriving (Typeable, Eq, Show)

instance Show PulseConfig where
    show (PulseConfig {..}) = 
        concat ["PulseConfig { "
               , "pServer = ", show _pServer
               , ", pApiKey = ", show _pApiKey
               , ", pAuth = ", show _pAuth
               , ", pManager = ", case _pManager of
                      Left _  -> "Left _"
                      Right _ -> "Right _"
               , " }"
               ]

