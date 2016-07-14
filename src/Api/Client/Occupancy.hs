{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module Api.Client.Occupancy where

import           Control.Monad.Except       (liftIO)
import           Control.Monad.Trans.Except (ExceptT)
import           Data.Aeson
import           Data.Proxy
import           GHC.Generics
import           Network.HTTP.Client        (Manager, defaultManagerSettings,
                                             newManager)
import           Network.HTTP.Media         ((//), (/:))
import           Servant.API
import           Servant.API.ContentTypes   (eitherDecodeLenient)
import           Servant.Client

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

-- | PlainTextJSON instructs the server to treat the text\plain application
-- type as application\json. This should only be used when one knows that the
-- API being accessed is advertising their responses improperly.
data PlainTextJSON

instance Accept PlainTextJSON where
  contentType _ = "text" // "plain" /: ("charset", "utf-8")

instance FromJSON a => MimeUnrender PlainTextJSON a where
  mimeUnrender _ = eitherDecodeLenient

-- | As with the server API type, we use ':>' to build a route out of
-- component types. e.g. this type accesses the '/local/slug/goes?here'
-- endpoint.
type ClientAPI = "local" :> "slug" :> "goes" :> QueryFlag "here"
                 :> Get '[PlainTextJSON] Resp

-- Boilerplate required to convert the API type into a function that can be
-- consumed by `client`
clientApi :: Proxy ClientAPI
clientApi = Proxy

getOccSensor :: Bool -> Manager -> BaseUrl -> ClientM Resp
getOccSensor = client clientApi

-- | This function takes a list of BaseUrls and produces a list of responses
-- wrapped in the ExceptT transformer stack.
getOccSensorList :: [BaseUrl] -> ExceptT ServantError IO [Resp]
getOccSensorList urls = do
  manager <- liftIO $ newManager defaultManagerSettings
  mapM (getOccSensor True manager) urls

-- A small helper function to convert URLs in string form (e.g. 192.168.1.1) to
-- a simple, Http BaseUrl on port 80
urlify :: [String] -> [BaseUrl]
urlify = map (\addr -> BaseUrl Http addr 80 "")