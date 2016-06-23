module Main where

import           Client
import           Types  (BaseUrl, Difference (..), IPAddr (..), Resp (..),
                         runExceptT)

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

main :: IO ()
main = do
  resps <- runExceptT $ getRespList urls
  case resps of
    Left err -> putStrLn $ "Error: " ++ show err
    Right respList -> print $ getDifference respList
