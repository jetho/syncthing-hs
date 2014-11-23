
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
    , liftEither
    , liftReader
    , liftInner
    , liftLeft
    , liftRight
    ) where

import Data.Typeable               (Typeable)
import qualified Data.Text         as T
import qualified Network.Wreq      as W
import Network.HTTP.Client         (Manager, ManagerSettings)
import Control.Monad.Trans.Either  (EitherT, left, right)
import Control.Monad.Trans.Reader  (ReaderT)
import Control.Monad.Trans.Class   (lift)
import Data.Aeson                  (Value)
import Control.Applicative         (Applicative)
import Data.ByteString.Lazy hiding (concat)


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
                   deriving (Functor , Applicative , Monad)

class (Monad m) => MonadPulse m where
    getMethod  :: W.Options -> String -> m (ByteString)
    postMethod :: W.Options -> String -> Value -> m (ByteString)

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
    | Unauthorized
    | OtherError String
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

liftEither :: Monad m => EitherT PulseError (ReaderT PulseConfig m) a -> PulseM m a
liftEither = PulseM

liftReader :: Monad m => (ReaderT PulseConfig m) a -> PulseM m a
liftReader = liftEither . lift

liftInner :: Monad m => m a -> PulseM m a
liftInner = liftEither . lift . lift

liftLeft :: Monad m => PulseError -> PulseM m a
liftLeft = liftEither . left

liftRight :: Monad m => a -> PulseM m a
liftRight = liftEither . right

