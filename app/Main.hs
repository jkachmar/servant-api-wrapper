module Main where

import           Control.Monad.Trans.Except (runExceptT)

import           Client
import           Types                      (BaseUrl, IPAddr (..))

addrs :: [IPAddr]
addrs = [ IPAddr "192.0.2.189"
        , IPAddr "192.0.2.188"
        , IPAddr "192.0.2.187"
        , IPAddr "192.0.2.186"
        ]

urls :: [BaseUrl]
urls = urlify addrs

main :: IO ()
main = do
  resps <- runExceptT $ getRespList urls
  case resps of
    Left err -> putStrLn $ "Error: " ++ show err
    Right respList -> print respList
