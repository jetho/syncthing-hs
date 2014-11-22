
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Network.Pulse.Types 
    ( PulseM(..)
    , MonadPulse(..)
    , PulseConfig(..)
    , Param
    , HttpMethod(..)
    , PulseRequest(..)
    , PulseError(..)
    , liftPulse
    ) where

import Data.Typeable              (Typeable)
import qualified Data.Text        as T
import qualified Network.Wreq     as W
import Network.HTTP.Client        (Manager, ManagerSettings)
import Control.Monad.Trans.Either (EitherT)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Reader
import Data.Aeson                 (Value)
import Control.Applicative
import Control.Monad.Error 
import Data.ByteString.Lazy hiding (concat, unpack)


-- | The Pulse configuration for specifying the Pulse server,
-- authentication, the API Key etc.
data PulseConfig = PulseConfig { 
      _pServer    :: T.Text
    , _pApiKey    :: Maybe T.Text
    , _pAuth      :: Maybe W.Auth
    , _pManager   :: Either ManagerSettings Manager
    }

-- | The PulseM Monad represents one or multiple Pulse requests.
newtype PulseM m a = PulseM { runPulse :: EitherT PulseError (ReaderT PulseConfig m) a }
                   deriving ( Functor
                            , Applicative 
                            , Monad
                            , MonadReader PulseConfig
                            , MonadError PulseError
                            )

class (Monad m) => MonadPulse m where
    getMethod  :: W.Options -> String -> m (ByteString)
    postMethod :: W.Options -> String -> Value -> m (ByteString)

liftPulse  = PulseM

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

