{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Client where

import           Control.Monad.Trans.Except (runExceptT)
import           Data.Aeson
import           Data.Proxy
import           GHC.Generics
import           Network.HTTP.Client        (Manager, defaultManagerSettings,
                                             newManager)
import           Servant.API
import           Servant.Client

--------------------------------------------------------------------------------

newtype IPAddr = IPAddr String deriving Show

addrs :: [IPAddr]
addrs = [ IPAddr "192.0.2.189"
        , IPAddr "192.0.2.188"
        , IPAddr "192.0.2.187"
        , IPAddr "192.0.2.186"
        ]

urls :: [BaseUrl]
urls = map go addrs
  where go (IPAddr a) = BaseUrl Http a 80 ""

data Difference = Difference
  { totalIn  :: Int
  , totalOut :: Int
  , netDiff  :: Int
  } deriving Show

--------------------------------------------------------------------------------

data Resp = Resp
  { serial    :: String
  , name      :: String
  , timestamp :: String
  , respIn    :: String
  , respOut   :: String
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

type ClientAPI = "local" :> "slug" :> "goes" :> QueryFlag "here"
                 :> Get '[JSON] Resp

clientApi :: Proxy ClientAPI
clientApi = Proxy

getResp :: Bool -> Manager -> BaseUrl -> ClientM Resp
getResp = client clientApi

--------------------------------------------------------------------------------

query :: Manager -> BaseUrl -> ClientM Resp
query = getResp True

run :: IO ()
run = do
  manager <- newManager defaultManagerSettings
  resps <- runExceptT $ mapM (query manager) urls
  print resps
