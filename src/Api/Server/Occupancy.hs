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

import           Config                      (App (..))
import           Models

type OccupancyAPI =
  "occupancy" :> Get '[JSON] [Entity Occ]
  :<|> Capture "building" String :> Capture "floor" String :> Get '[JSON] (Entity Occ)
  :<|> "occupancy" :> ReqBody '[JSON] Occ :> Post '[JSON] Int64

-- | The server that runs the OccupancyAPI.
cameraServer :: ServerT OccupancyAPI App
cameraServer = allOccs :<|> singleOcc :<|> createOcc

-- | Returns all occupancy models in the database.
allOccs :: App [Entity Occ]
allOccs =
    runDb (selectList ([] :: [Filter Occ]) [])

-- | Returns an occupancy model by name or throws a 404 error.
singleOcc :: String -> String -> App (Entity Occ)
singleOcc bldg lvl = do
    maybeOcc <- runDb (selectFirst [OccBuilding ==. bldg, OccLevel ==. lvl] [])
    case maybeOcc of
         Nothing ->
            throwError err404
         Just occ ->
            return occ

-- | Creates an occupancy model in the database.
createOcc :: Occ -> App Int64
createOcc p = do
    newOcc <- runDb (insert (Occ (occBuilding p) (occLevel p) (occAddr p)))
    return $ fromSqlKey newOcc

-- | Generates JavaScript to query the Occupancy API.
generateJavaScript :: IO ()
generateJavaScript =
    writeJSForAPI (Proxy :: Proxy OccupancyAPI) vanillaJS "./assets/occupancyApi.js"
