{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Bacon (
    loremBacon,
    LoremType(..)
) where


import Servant.Client
import Servant.API
import Data.Text
import Servant
import Network.HTTP.Client (Manager)
import Data.Bifunctor

data LoremType = MeatAndFiller | AllMeat

instance ToHttpApiData LoremType where
    toQueryParam MeatAndFiller = "meat-and-filler"
    toQueryParam AllMeat = "all-meat"

type API = QueryParam "type" LoremType :> QueryParam "paras" Int :> QueryParam "start-with-lorem" Bool :> Get '[JSON] [Text]

clientAPI :: Proxy API
clientAPI = Proxy

loremBacon' :: Maybe LoremType -> Maybe Int -> Maybe Bool -> ClientM [Text]
loremBacon' = client clientAPI

loremBacon :: Manager -> Maybe LoremType -> Maybe Int -> Maybe Bool -> IO (Maybe [Text])
loremBacon mng a b c = do
    let env = mkClientEnv mng (BaseUrl Https "baconipsum.com" 443 "api")
    res <- runClientM (loremBacon' a b c) env
    return $ case res of  
        Left _ -> Nothing
        Right a -> Just a
    -- return . pack . show $ res