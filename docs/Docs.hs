{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- module Docs where

import Lib
import Foo.Data
import Servant.Docs
import Servant.API
import qualified Data.Text as T
import Servant.Server
import Servant
import Data.Foldable (fold)
import Servant.API.WebSocket
import Control.Lens

main = writeFile "docs.md" . markdown . docs . pretty $ server 

-- instance ToSample User
instance ToCapture (Capture "first" T.Text) where
    toCapture _ = DocCapture "first" "The first name of the user."

instance ToSample Foo.Data.Post where
    toSamples _ = samples $ Post `fmap` [1, 4, 7, 194] <*> [4, 7, 92] <*> ["This is the post body", "This is another post body"]

instance ToSample User where
    toSamples _ = samples [
        User (UserName "Seva" "Leonidovich") 1,
        User (UserName "Foo" "Barovich") 42,
        User (UserName "Bar" "Fooovich") 69,
        User (UserName "Qux" "Quixovich") 8]

instance ToCapture (Capture "second" T.Text) where
    toCapture _ = DocCapture "second" "The second name of the user."

instance ToSample (TokenizedResponse [Foo.Data.Post]) where
    toSamples _ = samples $ TokenizedResponse `fmap` [1, 46, 193, 7] <*> [map snd $ toSamples (Proxy :: Proxy Foo.Data.Post)]

instance ToCapture (Capture "id" Int) where
    toCapture _ = DocCapture "id" "The id of the entity you want to query."

instance ToCapture (Capture "token" Int) where
    toCapture _ = DocCapture "token" "The token, recieved wit the previuos request"

instance HasDocs WebSocketPending where
    docsFor Proxy (endpoint, action) =
        docsFor (Proxy :: Proxy Raw) (endpoint, action & notes <>~ [DocNote "title" ["not title"]])