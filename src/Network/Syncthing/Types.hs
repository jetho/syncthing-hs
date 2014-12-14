
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Network.Syncthing.Types
    ( SyncResult 
    , SyncM(..)
    , MonadSync(..)
    , SyncConfig(..)
    , Param
    , HttpMethod(..)
    , SyncRequest(..)
    , DeviceError(..)
    , SyncError(..)
    , liftEither
    , liftReader
    , liftInner
    , liftLeft
    , liftRight
    , get
    , post
    , getRequest
    , postRequest
    ) where

import           Control.Applicative        (Applicative)
import           Control.Exception          (Exception)
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Either (EitherT, left, right)
import           Control.Monad.Trans.Reader (ReaderT)
import           Data.Aeson                 (Value, toJSON, ToJSON)
import           Data.ByteString.Lazy       (ByteString)
import qualified Data.Text                  as T
import           Data.Typeable              (Typeable)
import           Network.HTTP.Client        (Manager, ManagerSettings)
import qualified Network.Wreq               as W


-- | The result type of Syncthing requests.
type SyncResult a = Either SyncError a

-- | The SyncM Monad represents one or multiple Syncthing requests.
newtype SyncM m a = SyncM {
      runSyncthing :: EitherT SyncError (ReaderT SyncConfig m) a
    } deriving (Functor , Applicative , Monad)

class Monad m => MonadSync m where
    getMethod  :: W.Options -> String -> m ByteString
    postMethod :: W.Options -> String -> Value -> m ByteString

type Param = (T.Text, T.Text)

data HttpMethod =
      Get
    | Post Value
    deriving (Eq, Show)

data SyncRequest = SyncRequest {
      path   :: String
    , method :: HttpMethod
    , params :: [Param]
    } deriving (Eq, Show)

-- | The Syncthing configuration for specifying the Syncthing server,
-- authentication, the API Key etc.
data SyncConfig = SyncConfig {
      _pServer  :: T.Text
    , _pApiKey  :: Maybe T.Text
    , _pAuth    :: Maybe W.Auth
    , _pHttps   :: Bool
    , _pManager :: Either ManagerSettings Manager
    }

instance Show SyncConfig where
    show (SyncConfig {..}) =
        concat ["SyncConfig { "
               , "pServer = ", show _pServer
               , ", pApiKey = ", show _pApiKey
               , ", pAuth = ", show _pAuth
               , ", pHttps = ", show _pHttps
               , ", pManager = ", case _pManager of
                      Left _  -> "Left _"
                      Right _ -> "Right _"
               , " }"
               ]

data DeviceError =
      IncorrectLength
    | IncorrectCheckDigit
    deriving (Eq, Show)

data SyncError =
      ParseError String
    | NotAuthorized
    | CSRFError
    | NotFound
    | InvalidDeviceId DeviceError
    | NoSuchFolder
    deriving (Typeable, Eq, Show)

instance Exception SyncError

liftEither :: Monad m => EitherT SyncError (ReaderT SyncConfig m) a -> SyncM m a
liftEither = SyncM

liftReader :: Monad m => (ReaderT SyncConfig m) a -> SyncM m a
liftReader = liftEither . lift

liftInner :: Monad m => m a -> SyncM m a
liftInner = liftEither . lift . lift

liftLeft :: Monad m => SyncError -> SyncM m a
liftLeft = liftEither . left

liftRight :: Monad m => a -> SyncM m a
liftRight = liftEither . right

get :: HttpMethod
get = Get

post :: ToJSON a => a -> HttpMethod
post = Post . toJSON 

getRequest :: SyncRequest
getRequest = SyncRequest {
      path   = "/rest/ping"
    , method = get
    , params = []
    }

postRequest :: SyncRequest
postRequest = SyncRequest {
      path   = "/rest/ping"
    , method = post ()
    , params = []
    }

