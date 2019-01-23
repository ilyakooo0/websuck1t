{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Foo.Data 
    (
        user,
        post,
        newUser,
        createApp,
        createConfig,
        User(..),
        UserName(..),
        Config,
        updates,
        allUsers,
        App,
        UserId,
        PostId,
        Post(..),
        allPosts,
        addConnection,
        removeConnection,
        TokenizedResponse(..),
        Update(..),
        getToken
    ) where
    
import GHC.Conc
import Control.Concurrent
import GHC.Generics
import qualified Data.Map.Strict as M
import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.Aeson
import Control.Monad
import Control.Monad.Reader
import Servant.Server
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.Bacon
import Data.Bifunctor
import Control.Monad.Except
import Data.Maybe
import Control.Concurrent.STM.TVar
import System.Random
import Servant.API.WebSocket
import Network.WebSockets
import qualified Data.Set as S
import qualified Data.CyclicBuffer as CB
import Data.Set ((\\), union)
import Data.Monoid
import Debug.Trace

type App = ReaderT Config Handler 

class HasConfig c where
    getCfg :: c -> Config

allUsers :: App [User]
allUsers = ask >>= liftIO . fmap M.elems . readTVarIO . users

post :: PostId -> App Post
post pId = do
    cfg <- ask 
    u <- liftIO . atomically . fmap (M.lookup pId) . readTVar . posts $ cfg
    case u of
        Just u -> return u
        Nothing -> throwError err404

user :: UserId -> App User
user uId = do
    cfg <- ask 
    u <- liftIO . atomically . fmap (M.lookup uId) . readTVar . users $ cfg
    case u of
        Just u -> return u
        Nothing -> throwError err404

allPosts :: App (STM [Post])
allPosts = asks $ fmap M.elems . readTVar . posts

getToken :: App (STM Token)
getToken = asks $ fmap CB.lastToken . readTVar . updates

createConfig :: IO Config
createConfig = do 
    mng <- newManager tlsManagerSettings
    cfg <- atomically $ Config <$> newTVar 0 <*> newTVar 0 <*> newTVar M.empty <*> newTVar M.empty <*> return mng <*> newTVar M.empty <*> newTVar 0 <*> newTVar (CB.empty 20)
    forkIO . forever $ threadDelay 7000000 >> (runHandler . createApp cfg $ startAddingPosts 20 cfg >>= uncurry sendUpdate)
    return cfg

type Token = Int

data TokenizedResponse d = TokenizedResponse {
    token :: Token,
    response :: d
} deriving Generic

instance ToJSON d => ToJSON (TokenizedResponse d)

data Update = Update {
    added :: S.Set PostId,
    deleted :: S.Set PostId
} deriving (Generic, Show)

instance ToJSON Update

instance Semigroup Update where
    (Update a1 d1) <> (Update a2 d2) = Update added deleted
        where 
            added' = a1 `S.union` a2
            deleted' = d1 `S.union` d2
            inter = added' `S.intersection` deleted'
            added = added' \\ inter
            deleted = deleted' \\ inter

instance Monoid Update where
    mempty = Update S.empty S.empty

addConnection :: HasConfig c => c -> Connection -> STM ConnectionId
addConnection c conn = do
    let cfg = getCfg c
    lastID <- fmap (+1) . readTVar . lastConnection $ cfg
    flip writeTVar lastID . lastConnection $ cfg
    modifyTVar (connections cfg) $ M.insert lastID conn
    return lastID
        
removeConnection :: HasConfig c => c -> ConnectionId -> STM ()
removeConnection c cId = do
    let cfg = getCfg c
    modifyTVar (connections cfg) $ M.delete cId 



createApp :: (HasConfig c) => c -> ReaderT Config Handler a -> Handler a
createApp = flip runReaderT . getCfg

type UserId = Int
type PostId = Int
type ConnectionId = Int

data Config = Config {
    lastUserID :: TVar UserId,
    lastPostID :: TVar PostId,
    users :: TVar (M.Map Int User),
    posts :: TVar (M.Map Int Post),
    manager :: Manager,
    connections :: TVar (M.Map ConnectionId Connection),
    lastConnection :: TVar ConnectionId,
    updates :: TVar (CB.CyclicBuffer Update)
}

instance HasConfig Config where
    getCfg = id

-- startAddingUsers :: Config -> App ()
-- startAddingUsers cfg = do 
--     a <- liftIO $ loremBacon (manager cfg) (Just AllMeat) (Just 2) (Just False)
--     let t = fromMaybe ["None", "None"] a
--     liftIO . atomically $ newUser cfg (t !! 0) (t !! 1) 
--     return ()

startAddingPosts :: Int -> Config -> App (Token, Update)
startAddingPosts lim cfg = do 
    cfg <- ask
    let us = users cfg
    let ps = posts cfg

    a <- liftIO $ loremBacon (manager cfg) (Just AllMeat) (Just lim) (Just 0)
    g <- liftIO newStdGen
    let rns = randoms g
    
    let t = fromMaybe (repeat "None") a
    liftIO . atomically $ do
        uu <- readTVar us
        let uSize = M.size uu
        if uSize >= 1 then do
                pp <- readTVar ps
                let toDeleteIndex = head rns `mod` M.size pp
                let deletedKey = fst $ M.elemAt toDeleteIndex pp
                dd <- if M.size pp > 0 then writeTVar ps (M.deleteAt toDeleteIndex pp) >> return [deletedKey]
                    else return []
                let toAdd = max 0 $ lim - (M.size pp - 1)
                add <- fmap (map postId) $ forM (zip rns $ take toAdd t) $ \(rnd, t) -> newPost cfg (randomFrom rnd uu) t
                let upd =  Update (S.fromList add) (S.fromList dd)
                let uppT = updates cfg
                upp <- readTVar uppT
                let (token, upp') = CB.insert upp upd
                writeTVar uppT upp'
                return (token, upd)
            else return (0, mempty)
    
sendUpdate :: Token -> Update -> App ()
sendUpdate t u = do
    cfg <- ask
    conns <- fmap M.elems . liftIO . readTVarIO $ connections cfg
    liftIO $ forM conns $ flip sendTextData $ encode $ TokenizedResponse t u
    return ()

randomFrom :: Int -> M.Map k v -> k
randomFrom g as = M.keys as !! (abs g `mod` M.size as)  

newUser :: Config -> Text -> Text -> STM User
newUser cfg first second = do
    let lastID = lastUserID cfg
    let us = users cfg
    do
        id <- readTVar lastID
        writeTVar lastID (id+1)
        let user = User (UserName first second) id 
        oldUsers <- readTVar us
        writeTVar us $ M.insert id user oldUsers
        return user

newPost :: Config -> UserId -> Text -> STM Post
newPost cfg user body = do
    let lastId = lastPostID cfg 
    let ps = posts cfg
    do
        id <- readTVar lastId
        writeTVar lastId (id+1)
        let p = Post id user body 
        modifyTVar ps $ M.insert id p
        return p

data User = User {
    userName :: UserName,
    userId :: UserId
} deriving Generic

data Post = Post {
    postId :: PostId,
    postAuthor :: UserId,
    postBody :: Text
} deriving Generic

instance ToJSON User 
instance ToJSON Post

data UserName = UserName {
    firstName :: Text,
    secondName :: Text
} deriving (Generic)

instance ToJSON UserName

