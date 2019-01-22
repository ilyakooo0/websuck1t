module Main where

import Network.Wai.Handler.Warp (run)
import Servant.Server (serve)
import System.Environment
import Lib

main :: IO ()
main = do 
    env <- getEnvironment
    let port = maybe 2000 read $ lookup "PORT" env
    srv <- createServer
    run port $ serve server srv
