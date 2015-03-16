
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Network.Syncthing.Internal.Monad
    ( SyncResult
    , SyncM(..)
    , MonadSync(..)
    , syncthingIO
    , liftEither
    , liftReader
    , liftInner
    , liftLeft
    , liftRight
    ) where

import           Control.Applicative               (Applicative, (<$>))
import           Control.Lens                      ((^.))
import           Control.Exception                 (catch)
import           Control.Monad.Trans.Class         (lift)
import           Control.Monad.Trans.Either        (EitherT, left, right, runEitherT)
import           Control.Monad.Trans.Reader        (ReaderT, runReaderT)
import           Data.Aeson                        (Value)
import           Data.ByteString.Lazy              (ByteString)
import qualified Network.Wreq                      as W

import           Network.Syncthing.Internal.Config
import           Network.Syncthing.Internal.Error


-- | The result type of Syncthing requests.
type SyncResult a = Either SyncError a

-- | The SyncM Monad represents one or multiple Syncthing requests.
newtype SyncM m a = SyncM {
      runSyncthing :: EitherT SyncError (ReaderT SyncConfig m) a
    } deriving (Functor , Applicative , Monad)

class Monad m => MonadSync m where
    getMethod  :: W.Options -> String -> m ByteString
    postMethod :: W.Options -> String -> Value -> m ByteString

-- | Use Wreq's getWith and postWith functions when running in IO
instance MonadSync IO where
    getMethod  o s   = (^. W.responseBody) <$> W.getWith  o s
    postMethod o s p = (^. W.responseBody) <$> W.postWith o s p

-- | Run Syncthing requests in the IO Monad.
syncthingIO :: SyncConfig -> SyncM IO a -> IO (SyncResult a)
syncthingIO config action =
    runReaderT (runEitherT $ runSyncthing action) config `catch` syncErrHandler

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

