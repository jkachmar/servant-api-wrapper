{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Api.Server.Occupancy where

import           Control.Monad.Except
import           Data.Int                    (Int64)
import           Database.Persist.Postgresql (Entity (..), Filter, fromSqlKey,
                                              insert, selectFirst, selectList,
                                              (==.))
import           Servant
import           Servant.JS                  (vanillaJS, writeJSForAPI)

import           Api.Client.Occupancy
import           Config                      (App (..))
import           Models

type OccupancyAPI =
  "occupancy" :> Get '[JSON] [Entity Occ]
  :<|> Capture "building" String :> Capture "floor" String :> Get '[JSON] (Entity Occ)
  :<|> "occupancy" :> ReqBody '[JSON] Occ :> Post '[JSON] Int64

-- | The server that runs the OccupancyAPI.
cameraServer :: ServerT OccupancyAPI App
cameraServer = allOccs :<|> getDifference :<|> createOcc

-- | Returns all occupancy models in the database.
allOccs :: App [Entity Occ]
allOccs =
  runDb (selectList ([] :: [Filter Occ]) [])

-- | Gets the net occupancy of a floor within a building.
getDifference :: String -> String -> App (Entity Occ)
getDifference bldg lvl = do
  maybeOcc <- runDb (selectFirst [OccBuilding ==. bldg, OccLevel ==. lvl] [])
  case maybeOcc of
    Nothing ->
      throwError err404
    Just occ ->
      return occ
  where go :: Resp -> (Int, Int)
        go resp = (,) (respIn resp) (respOut resp)

-- | Creates an occupancy model in the database.
createOcc :: Occ -> App Int64
createOcc p = do
  newOcc <- runDb (insert (Occ (occBuilding p) (occLevel p) (occAddr p)))
  return $ fromSqlKey newOcc

-- | Generates JavaScript to query the Occupancy API.
generateJavaScript :: IO ()
generateJavaScript =
    writeJSForAPI
        (Proxy :: Proxy OccupancyAPI)
        vanillaJS
        "./assets/occupancyApi.js"
