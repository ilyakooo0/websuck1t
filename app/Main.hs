module Main where

import Network.Wai.Handler.Warp (run)
import Servant.Server (serve)
import Lib

main :: IO ()
main = do 
    srv <- createServer
    run 2000 $ serve server srv
