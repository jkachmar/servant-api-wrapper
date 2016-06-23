{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module Client (getRespList, urlify) where

import           Control.Monad.Except       (liftIO)
import           Control.Monad.Trans.Except (ExceptT)
import           Data.Aeson
import           Data.Proxy
import           Network.HTTP.Client        (Manager, defaultManagerSettings,
                                             newManager)
import           Network.HTTP.Media         ((//), (/:))
import           Servant.API
import           Servant.API.ContentTypes   (eitherDecodeLenient)
import           Servant.Client

import           Types                      (Difference (..), IPAddr (..),
                                             Resp (..))

--------------------------------------------------------------------------------
-- This is only necessary if the endpoint improperly encodes JSON as
-- text\plain rather than application\json

data PlainTextJSON

instance Accept PlainTextJSON where
  contentType _ = "text" // "plain" /: ("charset", "utf-8")

instance FromJSON a => MimeUnrender PlainTextJSON a where
  mimeUnrender _ = eitherDecodeLenient

--------------------------------------------------------------------------------
-- Construct Servant BaseUrls given a list of IPs

urlify :: [IPAddr] -> [BaseUrl]
urlify = map go
  where go (IPAddr a) = BaseUrl Http a 80 ""

--------------------------------------------------------------------------------

type ClientAPI = "local" :> "slug" :> "goes" :> QueryFlag "here"
                 :> Get '[PlainTextJSON] Resp

clientApi :: Proxy ClientAPI
clientApi = Proxy

getResp :: Bool -> Manager -> BaseUrl -> ClientM Resp
getResp = client clientApi

--------------------------------------------------------------------------------

query :: Manager -> BaseUrl -> ClientM Resp
query = getResp True

getRespList :: [BaseUrl] -> ExceptT ServantError IO [Resp]
getRespList urls = do
  manager <- liftIO $ newManager defaultManagerSettings
  mapM (query manager) urls

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
