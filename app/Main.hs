module Main where

import Network.Wai.Handler.Warp (run)
import Servant.Server (serve)
import System.Environment
import Lib
import Network.Wai.Middleware.Cors

main :: IO ()
main = do 
    env <- lookupEnv "PORT"
    let port = maybe 2000 read env :: Int
    putStrLn $ "Starting on port: " ++ show port
    print env
    srv <- createServer
    run port $ simpleCors $ serve server srv
