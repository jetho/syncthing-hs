{-# LANGUAGE DeriveDataTypeable #-}

module Network.Pulse.Types 
    ( Param
    , HttpMethod
    , PulseRequest(..)
    , PulseError(..)
    ) where

import Data.Typeable (Typeable)
import Data.Text     as T


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
