
{-# OPTIONS_HADDOCK show-extensions, not-home #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Network.Syncthing.Internal.Monad
    ( SyncResult 
    , SyncM(..)
    , MonadSync(..)
    , liftEither
    , liftReader
    , liftInner
    , liftLeft
    , liftRight
    ) where

import           Control.Applicative        (Applicative)
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Either (EitherT, left, right)
import           Control.Monad.Trans.Reader (ReaderT)
import           Data.Aeson                 (Value)
import           Data.ByteString.Lazy       (ByteString)
import qualified Network.Wreq               as W

import Network.Syncthing.Internal.Config
import Network.Syncthing.Internal.Error


-- | The result type of Syncthing requests.
type SyncResult a = Either SyncError a

-- | The SyncM Monad represents one or multiple Syncthing requests.
newtype SyncM m a = SyncM {
      runSyncthing :: EitherT SyncError (ReaderT SyncConfig m) a
    } deriving (Functor , Applicative , Monad)

class Monad m => MonadSync m where
    getMethod  :: W.Options -> String -> m ByteString
    postMethod :: W.Options -> String -> Value -> m ByteString

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

