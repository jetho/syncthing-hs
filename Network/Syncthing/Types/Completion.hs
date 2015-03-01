
{-# LANGUAGE OverloadedStrings #-}

module Network.Syncthing.Types.Completion
    ( Completion(..)
    ) where

import           Control.Applicative              ((<$>))
import           Control.Monad                    (MonadPlus (mzero))
import           Data.Aeson                       (FromJSON, Value (..), parseJSON, (.:))


newtype Completion = Completion { getCompletion :: Int } 
                     deriving (Eq, Show)

instance FromJSON Completion where
    parseJSON (Object v) = Completion <$> (v .: "completion")
    parseJSON _          = mzero

