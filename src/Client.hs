{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module Client where

import           Control.Monad.Except       (liftIO)
import           Control.Monad.Trans.Except (ExceptT, runExceptT)
import           Data.Aeson
import           Data.Proxy
import           GHC.Generics
import           Network.HTTP.Client        (Manager, defaultManagerSettings,
                                             newManager)
import           Network.HTTP.Media         ((//), (/:))
import           Servant.API
import           Servant.API.ContentTypes   (eitherDecodeLenient)
import           Servant.Client

--------------------------------------------------------------------------------
-- This is only necessary if the endpoint improperly encodes JSON as
-- text\plain rather than application\json

data PlainTextJSON

instance Accept PlainTextJSON where
  contentType _ = "text" // "plain" /: ("charset", "utf-8")

instance FromJSON a => MimeUnrender PlainTextJSON a where
  mimeUnrender _ = eitherDecodeLenient

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

type ClientAPI = "local" :> "slug" :> "goes" :> QueryFlag "here"
                 :> Get '[PlainTextJSON] Resp

clientApi :: Proxy ClientAPI
clientApi = Proxy

getResp :: Bool -> Manager -> BaseUrl -> ClientM Resp
getResp = client clientApi

--------------------------------------------------------------------------------

query :: Manager -> BaseUrl -> ClientM Resp
query = getResp True

getRespList :: ExceptT ServantError IO [Resp]
getRespList = do
  manager <- liftIO $ newManager defaultManagerSettings
  mapM (query manager) urls

run :: IO ()
run = do
  resps <- runExceptT getRespList
  case resps of
    Left err -> putStrLn $ "Error: " ++ show err
    Right respList -> print $ getDifference respList

--------------------------------------------------------------------------------
-- all of these functions would be nicer if this used lenses..

getDifference :: [Resp] -> Difference
getDifference resps =
  let listIn  = sum $ fst <$> getTotalInOut resps
      listOut = sum $ snd <$> getTotalInOut resps
  in Difference { totalIn = listIn
                , totalOut = listOut
                , netDiff = listIn - listOut
                }


getTotalInOut :: [Resp] -> [(Int, Int)]
getTotalInOut = map go
  where go resp =
          let totIn = respIn resp
              totOut = respOut resp
          in (totIn, totOut)
