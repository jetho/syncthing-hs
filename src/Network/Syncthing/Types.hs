
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Network.Syncthing.Types
    ( SyncthingM(..)
    , MonadST(..)
    , SyncthingConfig(..)
    , Param
    , HttpMethod(..)
    , SyncthingRequest(..)
    , SyncthingError(..)
    , decodeError
    , liftEither
    , liftReader
    , liftInner
    , liftLeft
    , liftRight
    ) where

import           Control.Applicative        (Applicative)
import           Control.Exception          (Exception)
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Either (EitherT, left, right)
import           Control.Monad.Trans.Reader (ReaderT)
import           Data.Aeson                 (Value)
import           Data.ByteString.Lazy       hiding (concat)
import qualified Data.Text                  as T
import           Data.Typeable              (Typeable)
import           Network.HTTP.Client        (Manager, ManagerSettings)
import qualified Network.Wreq               as W


-- | The SyncthingM Monad represents one or multiple Syncthing requests.
newtype SyncthingM m a = SyncthingM { runSyncthing :: EitherT SyncthingError (ReaderT SyncthingConfig m) a }
                   deriving (Functor , Applicative , Monad)

class Monad m => MonadST m where
    getMethod  :: W.Options -> String -> m ByteString
    postMethod :: W.Options -> String -> Value -> m ByteString

type Param = (T.Text, T.Text)

data HttpMethod =
      Get
    | Post Value
    deriving (Eq, Show)

data SyncthingRequest = SyncthingRequest {
      path   :: String
    , method :: HttpMethod
    , params :: [Param]
    } deriving (Eq, Show)

-- | The Syncthing configuration for specifying the Syncthing server,
-- authentication, the API Key etc.
data SyncthingConfig = SyncthingConfig {
      _pServer  :: T.Text
    , _pApiKey  :: Maybe T.Text
    , _pAuth    :: Maybe W.Auth
    , _pHttps   :: Bool
    , _pManager :: Either ManagerSettings Manager
    }

instance Show SyncthingConfig where
    show (SyncthingConfig {..}) =
        concat ["SyncthingConfig { "
               , "pServer = ", show _pServer
               , ", pApiKey = ", show _pApiKey
               , ", pAuth = ", show _pAuth
               , ", pHttps = ", show _pHttps
               , ", pManager = ", case _pManager of
                      Left _  -> "Left _"
                      Right _ -> "Right _"
               , " }"
               ]

data SyncthingError =
      ParseError String
    | NotAuthorized
    | CSRFError
    | NotFound
    deriving (Typeable, Eq, Show)

instance Exception SyncthingError

decodeError :: ByteString -> Maybe SyncthingError
decodeError = flip lookup
    [ ("CSRF Error\n",          CSRFError)
    , ("Not Authorized\n",      NotAuthorized)
    , ("404 page not found\n",  NotFound)
    ]

liftEither :: Monad m => EitherT SyncthingError (ReaderT SyncthingConfig m) a -> SyncthingM m a
liftEither = SyncthingM

liftReader :: Monad m => (ReaderT SyncthingConfig m) a -> SyncthingM m a
liftReader = liftEither . lift

liftInner :: Monad m => m a -> SyncthingM m a
liftInner = liftEither . lift . lift

liftLeft :: Monad m => SyncthingError -> SyncthingM m a
liftLeft = liftEither . left

liftRight :: Monad m => a -> SyncthingM m a
liftRight = liftEither . right

