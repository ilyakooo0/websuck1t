{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

type API =   "users" :> Capture "id" UserId :> Get '[JSON] User
        :<|> "users" :> "add" :> Capture "first" Text :> Capture "second" Text :> Get '[JSON] [User]
        :<|> "users" :> "all" :> Get '[JSON] [User]
        :<|> "posts" :> Capture "id" PostId :> Get '[JSON] D.Post
        :<|> "posts" :> "all" :> Get '[JSON] (TokenizedResponse Update)
        :<|> "posts" :> "subscribe" :> Capture "token" Int :> WebSocketPending
        
createServer :: P.Connection -> IO (Server API)
createServer pConn = do
    cfg <- createConfig pConn
    -- newUser cfg "Kostya" "Designerovich"
    -- newUser cfg "Anna" "Androidovna"
    -- newUser cfg "Darya" "Aiphonovna"
    -- newUser cfg "Ilya" "Serverovich"
    -- newUser cfg "Константин" "Дудосерович"
    return $ hoistServer server (createApp cfg) serverAPI

server :: Proxy API
server = Proxy
 
serverAPI :: ServerT API App
serverAPI = 
         user
    :<|> createUserPath
    :<|> allUsers
    :<|> post
    :<|> returnPosts
    :<|> validateConnection

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

invalidTokenError :: RejectRequest
invalidTokenError = defaultRejectRequest {
    rejectCode = 498,
    rejectMessage = "The supplied token is not valid" }

validateConnection :: Int -> PendingConnection -> App ()
validateConnection t pc = do
    cfg <- ask
    (cyc, token) <-  liftIO $ atomically $ do 
        uu <- readTVar $ updates cfg
        return (getFrom uu t, lastToken uu)
    case cyc of
        Nothing -> liftIO $ rejectRequestWith pc invalidTokenError
        Just uu -> do
            conn <- liftIO $ acceptRequest pc
            liftIO $ sendTextData conn $ encode $ TokenizedResponse token $ fold uu
            subscribe conn
            return ()

subscribe :: Connection -> App ()
subscribe conn = do
    cfg <- ask
    cId <- liftSTM $ addConnection cfg conn
    -- liftIO $ sendDataMessage conn . Binary . encode $ D.TokenizedResponse 1 (Update (S.fromList [1, 2, 3]) (S.fromList [4, 2,6]))
    liftIO $ flip finally (atomically $ removeConnection cfg cId) $ forever $ do 
        _ <- receiveDataMessage conn
        return ()
    return ()

liftSTM :: MonadIO m => STM a -> m a
liftSTM = liftIO . atomically