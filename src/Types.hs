{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Types (Difference(..), IPAddr(..), Resp(..)) where

import           Data.Aeson
import           GHC.Generics

newtype IPAddr = IPAddr String deriving Show

data Difference = Difference
  { totalIn  :: Int
  , totalOut :: Int
  , netDiff  :: Int
  } deriving (Show, Generic)

instance ToJSON Difference

data Resp = Resp
  { serial    :: String
  , name      :: String
  , timestamp :: String
  , respIn    :: Int
  , respOut   :: Int
  } deriving (Show, Generic)

instance ToJSON Resp
instance FromJSON Resp where
  parseJSON (Object v) = Resp
                         <$> v .: "serial"
                         <*> v .: "name"
                         <*> v .: "timestamp"
                         <*> v .: "in"
                         <*> v .: "out"
  parseJSON _          = mempty
