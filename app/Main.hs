-- {-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wai.Handler.Warp (run)
import Servant.Server (serve)
import System.Environment
import Lib
import Network.Wai.Middleware.Cors
import Database.Beam.Postgres (connectPostgreSQL)
import Data.Maybe
import qualified Data.ByteString.Char8 as B

main :: IO ()
main = do 
    mPort <- lookupEnv "PORT"
    let port = maybe 2000 read mPort :: Int
    putStrLn $ "Starting on port: " ++ show port
    mDatabaseURL <- lookupEnv "DATABASE_URL"
    let databaseURL = fromMaybe "host=localhost port=5432 dbname=postgres connect_timeout=10" mDatabaseURL
    conn <- connectPostgreSQL $ B.pack databaseURL
    srv <- createServer conn
    run port $ simpleCors $ serve server srv
