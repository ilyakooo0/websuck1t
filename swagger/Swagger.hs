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
import Data.Swagger
import Servant.Swagger
import Servant.API
import Data.Text
import Servant.Server
import Data.Aeson
import qualified Data.ByteString.Lazy as BS

instance ToSchema User
-- instance ToSchema UserName

main = return () -- BS.writeFile "swag.txt" . encode . toSwagger $ server