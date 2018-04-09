module Main where

import           Router
import           Snap.Core        (route)
import           Snap.Http.Server (defaultConfig, httpServe)

main :: IO ()
main = do
  putStrLn "Starting server on 8080"
  httpServe defaultConfig (route [("api", Router.routes)])
