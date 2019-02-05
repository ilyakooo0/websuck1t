{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}


module Lib
    ( server,
    createServer

    ) where

import Data.Proxy
import Servant.API
import Servant
import GHC.Conc
import Control.Monad.IO.Class
import System.IO
import Foo.Data as D
import Data.Text
import Control.Monad.Reader
import Network.Bacon
import Servant.API.WebSocket
import Network.WebSockets
import Control.Monad (forever)
import Control.Exception (finally)
import Data.Aeson
import qualified Data.Set as S
import Data.CyclicBuffer
import Data.Foldable (fold)
import qualified Database.Beam.Postgres as P
-- import Network.Wai
import Network.EngineIO.Wai (toWaiApplication, waiAPI)
import Network.SocketIO
import Control.Monad.State
import Debug.Trace

type API =   "users" :> Capture "id" UserId :> Get '[JSON] User
        :<|> "users" :> "add" :> Capture "first" Text :> Capture "second" Text :> Get '[JSON] [User]
        :<|> "users" :> "all" :> Get '[JSON] [User]
        :<|> "posts" :> Capture "id" PostId :> Get '[JSON] D.Post
        :<|> "posts" :> "all" :> Get '[JSON] (TokenizedResponse Update)
        :<|> "posts" :> "subscribe" :> Raw --Capture "token" Int :> WebSocketPending
        
createServer :: P.Connection -> IO (Server API)
createServer pConn = do
    cfg <- createConfig pConn
    -- newUser cfg "Kostya" "Designerovich"
    -- newUser cfg "Anna" "Androidovna"
    -- newUser cfg "Darya" "Aiphonovna"
    -- newUser cfg "Ilya" "Serverovich"
    -- newUser cfg "Константин" "Дудосерович"
    subscribePostsEndpoint <- subscribePosts cfg
    let app = toWaiApplication subscribePostsEndpoint :: Application
    let sAPI = serverAPI app
    return $ hoistServer server (createApp cfg) sAPI

server :: Proxy API
server = Proxy
 
serverAPI :: Application -> ServerT API App
serverAPI app =  user
                :<|> createUserPath
                :<|> allUsers
                :<|> post
                :<|> returnPosts
                :<|> pure app
    -- :<|> validateConnection

createUserPath :: Text -> Text -> App [User]
createUserPath first second = do 
    cfg <- ask
    liftIO $ newUser cfg first second 
    allUsers

returnPosts :: App (TokenizedResponse Update)
returnPosts = do 
    posts <- allPosts 
    t <- getToken 
    liftIO $ atomically $ TokenizedResponse <$> t <*> (pure $ flip Update (S.empty) $ S.fromList $ Prelude.map D.postId posts)

-- invalidTokenError :: RejectRequest
-- invalidTokenError = defaultRejectRequest {
--     rejectCode = 498,
--     rejectMessage = "The supplied token is not valid" }

-- validToken :: Token -> App Bool
-- validToken t pc = do
--     cfg <- ask
--     (cyc, token) <-  liftIO $ atomically $ do 
--         uu <- readTVar $ updates cfg
--         return (getFrom uu t, lastToken uu)
--     return $ case cyc of
--         Nothing -> False
--         Just uu -> True



-- subscribePosts :: Config -> IO (m())
subscribePosts cfg = do
    handle <- initialize waiAPI $ subscriptionHandler cfg
    return handle

-- subscriptionHandler :: MonadIO m => Config -> StateT RoutingTable (ReaderT Socket m) a 
subscriptionHandler :: MonadState RoutingTable m => Config -> m ()
subscriptionHandler cfg = do 
    on "test" $ \ (i::Int) -> do
        emit "test" $ Update (S.fromList [1,2,3]) (S.fromList [4,5,6])

    appendDisconnectHandler $ do
        conn <- ask
        liftSTM $ removeConnection cfg conn

    
    on D.subscribeWithToken $ \ (t :: Token) -> do
        -- emit "subscribeWithToken" $ do
        conn <- ask
        (cyc, token) <-  liftIO $ atomically $ do 
            uu <- readTVar $ updates cfg
            return (getFrom uu t, lastToken uu)
        case cyc of
            Nothing -> emit D.subscribeWithToken False
            Just uu -> do
                emit D.postUpdates $ TokenizedResponse token $ fold uu
                sId <- liftSTM $ addConnection cfg conn
                emit subscribeWithToken True
       
            -- if validToken t then do
            --     conn <- ask
            --     liftSTM $ do
            --         subscribe cfg conn
            --         uu <- readTVar $ updates cfg
            --         return (lastToken uu)
            
            --     emit "postUpdates"
            -- else return false

    
            
liftSTM :: MonadIO m => STM a -> m a
liftSTM = liftIO . atomically