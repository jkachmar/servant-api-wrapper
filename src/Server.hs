{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Server
    ( startApp
    ) where

import           Control.Monad.Except       (liftIO)
import           Control.Monad.IO.Class     (MonadIO)
import           Control.Monad.Trans.Except (ExceptT (..), runExceptT, throwE)
import           Network.HTTP.Types         (Status (..))
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant                    ((:>), Get, JSON, Proxy (..),
                                             ServantErr (..), Server, err404,
                                             serve)
import           Servant.Client             (BaseUrl)
import           Servant.Common.Req         (ServantError (..))

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
-- this would still probably be nicer with lenses...

getDifference :: [Resp] -> Difference
getDifference resps =
  let listIn  = sum $ fst <$> map go resps
      listOut = sum $ snd <$> map go resps

  in Difference listIn listOut (listIn - listOut)

  where go :: Resp -> (Int, Int)
        go resp = (,) (respIn resp) (respOut resp)

getDifferences :: ExceptT ServantErr IO Difference
getDifferences = let resps = liftError (getRespList urls)
                 in getDifference <$> resps

--------------------------------------------------------------------------------

type ServerAPI = "occupancy" :> Get '[JSON] Difference

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve serverApi server

serverApi :: Proxy ServerAPI
serverApi = Proxy

server :: Server ServerAPI
server = getDifferences

--------------------------------------------------------------------------------
-- Convert client errors to server errors

liftError :: (Show b, MonadIO m) => ExceptT ServantError m b -> ExceptT ServantErr m b
liftError apiReq = either
  logAndFail
  return =<< ExceptT
  (Right <$> runExceptT apiReq)

logAndFail :: (Show b, MonadIO m) => ServantError -> ExceptT ServantErr m b
logAndFail e = do
  liftIO (putStrLn ("Got internal api error: " ++ show e))
  throwE (convertError e)

convertError :: ServantError -> ServantErr
convertError  (FailureResponse (Status code body) _ _) =
  ServantErr code (show body) "" []

convertError (UnsupportedContentType _ _) =
  err404 { errBody = "Resource not found"}

convertError _ =
  ServantErr 500 "Internal Server Error" "" []
