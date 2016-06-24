{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Server
    ( startApp
    ) where

import           Control.Monad.Except       (liftIO)
import           Control.Monad.IO.Class     (MonadIO)
import           Control.Monad.Trans.Except (ExceptT (..), runExceptT, throwE)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant                    ((:>), Get, JSON, Proxy (..),
                                             ServantErr (..), Server, serve)
import           Servant.Client             (BaseUrl)
import           Servant.Common.Req         (ServantError)

import           Client                     (getRespList, urlify)
import           Types

--------------------------------------------------------------------------------

addrs :: [IPAddr]
addrs = [ IPAddr "192.0.2.189"
        , IPAddr "192.0.2.188"
        , IPAddr "192.0.2.187"
        , IPAddr "192.0.2.186"
        ]

urls :: [BaseUrl]
urls = urlify addrs

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

--------------------------------------------------------------------------------

type API = "occupancy" :> Get '[JSON] [Resp]

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = getOccupancy

getOccupancy :: ExceptT ServantErr IO [Resp]
getOccupancy = convertError $ getRespList urls

convertError :: (Show b, MonadIO m) => ExceptT ServantError m b -> ExceptT ServantErr m b
convertError apiReq = either logAndFail return =<< ExceptT (fmap Right (runExceptT apiReq))
  where
    logAndFail e = do
      liftIO (putStrLn ("Got internal api error: " ++ show e))
      throwE internalError
    internalError = ServantErr 500 "Internal Server Error" "" []
