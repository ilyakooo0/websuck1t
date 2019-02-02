{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module Foo.Data 
    (
        user,
        post,
        newUser,
        createApp,
        createConfig,
        User(..),
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
import Database.Beam
import qualified Database.Beam.Postgres as P
import Database.Beam.Query
import Database.PostgreSQL.Simple (withTransaction)
import Database.Beam.Backend.SQL.BeamExtensions
import System.Random.Shuffle

type App = ReaderT Config Handler 

class HasConfig c where
    getCfg :: c -> Config

allUsers :: App [User]
allUsers = asks dbConn >>= \conn -> liftIO . P.runBeamPostgres conn . runSelectReturningList . select . all_ . users $ socialDb

post :: PostId -> App Post
post pId = do
    conn <- asks dbConn
    u <- liftIO . P.runBeamPostgres conn . runSelectReturningOne . flip lookup_ (PostId pId) . posts $ socialDb
    case u of
        Just u -> return u
        Nothing -> throwError err404

user :: UserId -> App User
user uId = do
    conn <- asks dbConn
    u <- liftIO . P.runBeamPostgres conn . runSelectReturningOne . flip lookup_ (UserId uId) . users $ socialDb
    case u of
        Just u -> return u
        Nothing -> throwError err404

allPosts :: App [Post]
allPosts = asks dbConn >>= \conn -> liftIO . P.runBeamPostgres conn . runSelectReturningList . select . all_ . posts $ socialDb

getToken :: App (STM Token)
getToken = asks $ fmap CB.lastToken . readTVar . updates

createConfig :: P.Connection -> IO Config
createConfig conn = do 
    mng <- newManager tlsManagerSettings
    cfg <- atomically $ Config <$> return conn <*> return mng <*> newTVar M.empty <*> newTVar 0 <*> newTVar (CB.empty 20)
    forkIO . forever $ threadDelay 7000000 >> (runHandler . createApp cfg $ startAddingPosts 25 cfg >>= uncurry sendUpdate)
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
    dbConn :: P.Connection,
    manager :: Manager,
    connections :: TVar (M.Map ConnectionId Connection),
    lastConnection :: TVar ConnectionId,
    updates :: TVar (CB.CyclicBuffer Update)
}

instance HasConfig Config where
    getCfg = id

startAddingPosts :: Int -> Config -> App (Token, Update)
startAddingPosts lim cfg = do 
    cfg <- ask
    let conn = dbConn cfg
    -- let us = users socialDb
    -- let ps = posts socialDb

    a <- liftIO $ loremBacon (manager cfg) (Just AllMeat) (Just lim) (Just 0)
    g <- liftIO newStdGen
    let rns = randoms g
   
    let t = fromMaybe (repeat "None") a

    let deleteCount = 3
    
    (deletedIds, addedIds) <- join . liftIO . withTransaction conn . return $ do
        userIds <- liftIO . P.runBeamPostgres conn . runSelectReturningList . select $ do
            uc <- all_ . users $ socialDb
            pure (userId uc)

        if length userIds <= 0 then return ([],[])
        else do
            postIds <- liftIO . P.runBeamPostgres conn . runSelectReturningList . select $ do 
                uc <- all_ . posts $ socialDb
                pure (postId uc)
            

            let postCount = length postIds
            let toDeleteIndecies = take deleteCount $ shuffle' postIds postCount g

            let toAdd = max 0 $ lim - length postIds + length toDeleteIndecies

            -- Just deletedPost <- liftIO . P.runBeamPostgres conn . runSelectReturningOne . select . limit_ 1 . offset_ (fromIntegral $ toDeleteIndex - 1)  . all_ . posts $ socialDb
            -- let deletedId = postId deletedPost
                
            unless (length toDeleteIndecies <= 0) $
                liftIO . P.runBeamPostgres conn . runDelete $ 
                delete (posts socialDb) ((`in_` map val_ toDeleteIndecies) . postId)
            
            nIds <- liftIO . P.runBeamPostgres conn . runInsertReturningList (posts socialDb) $ insertExpressions $ take toAdd . flip map (zip rns t) $ \(r, t) -> Post default_ (val_ (userIds %! r)) (val_ t)

            return (toDeleteIndecies, map postId nIds)

    liftIO . atomically $ do 
        let upd =  Update (S.fromList addedIds) (S.fromList deletedIds)
        let uppT = updates cfg
        upp <- readTVar uppT
        let (token, upp') = CB.insert upp upd
        writeTVar uppT upp'
        return (token, upd)
    
sendUpdate :: Token -> Update -> App ()
sendUpdate t u = do
    cfg <- ask
    conns <- fmap M.elems . liftIO . readTVarIO $ connections cfg
    liftIO $ forM conns $ flip sendTextData $ encode $ TokenizedResponse t u
    return ()

modIndex :: [a] -> Int -> a
modIndex aa i = aa !! (i `mod` length aa)

infixl 5 %!
(%!) = modIndex


randomFrom :: Int -> M.Map k v -> k
randomFrom g as = M.keys as !! (abs g `mod` M.size as)  

newUser :: Config -> Text -> Text -> IO [User]
newUser cfg first second = do
    let conn = dbConn cfg
    P.runBeamPostgres conn . runInsertReturningList (users socialDb) $ insertExpressions  [User default_ (val_ first) (val_ second)]


-- newPost :: Config -> UserId -> Text -> STM Post
-- newPost cfg user body = do
--     let lastId = lastPostID cfg 
--     let ps = posts cfg
--     do
--         id <- readTVar lastId
--         writeTVar lastId (id+1)
--         let p = Post id user body 
--         modifyTVar ps $ M.insert id p
--         return p

data UserT f = User {
    userId :: Columnar f UserId,
    firstName :: Columnar f Text,
    secondName :: Columnar f Text
} deriving Generic

instance Beamable UserT

type User = UserT Identity

instance Table UserT where
    data PrimaryKey UserT f = UserId (Columnar f UserId) 
        deriving Generic

    primaryKey = UserId . userId

instance Beamable (PrimaryKey UserT)

data PostT f = Post {
    postId :: Columnar f PostId,
    postAuthor :: Columnar f UserId,
    postBody :: Columnar f Text
} deriving Generic

instance Beamable PostT

type Post = PostT Identity

instance ToJSON User 
instance ToJSON Post

instance Table PostT where
    data PrimaryKey PostT f = PostId (Columnar f PostId) 
        deriving Generic

    primaryKey = PostId . postId

instance Beamable (PrimaryKey PostT)

data SocialDb f = SocialDb {
    users :: f (TableEntity UserT),
    posts :: f (TableEntity PostT)
} deriving Generic

instance Database be SocialDb

socialDb :: DatabaseSettings be SocialDb
socialDb = defaultDbSettings `withDbModification` 
    dbModification {
        users = modifyTable id $ tableModification {
            firstName = "firstname",
            secondName = "secondname"
        }
    }