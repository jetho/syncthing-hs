{-# LANGUAGE OverloadedStrings #-}

module Pulse where

import Network.Wreq
import Control.Monad.Trans.Either 
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Data.Aeson
import Data.Text


type Pulse a = EitherT PulseError (ReaderT PulseContext IO) a

type Param = (Text, Text)

data Method = 
      Get 
    | Post
    deriving (Eq, Show)

data PulseConfig = PulseConfig
    { url       :: Text
    , apiKey    :: Maybe Text
    }

data PulseContext = PulseContext
    { config    :: PulseConfig
    , options   :: Options
    }

data PulseRequest = PulseRequest
    { method    :: Method
    , params    :: [Param]
    }
    
data PulseError = InvalidApiKey
                deriving (Eq, Show)

defaultPulseConfig :: PulseConfig
defaultPulseConfig = undefined

query :: FromJSON a => PulseRequest -> Pulse a
query = undefined


pulse :: (MonadIO m, FromJSON a) => PulseConfig -> Pulse a -> m (Either PulseError a)
pulse = undefined




