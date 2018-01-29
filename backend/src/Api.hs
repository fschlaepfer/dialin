{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Api
    ( Api
    ) where

import           Models

import           Data.Int (Int64)
import           Data.Text (Text)
import           Servant              

type Api m =
--         "person" :> ReqBody '[JSON] Person :> Post '[JSON] (Maybe (Key Person))
--    :<|> "person" :> Capture "name" Text :> Get '[JSON] (Maybe Person)
         "bean" :> ReqBody '[JSON] Bean :> Post '[JSON] (Maybe Int64)
    :<|> "bean" :> Capture "id" Int64 :> Get '[JSON] (Maybe Bean)
    :<|> "brew" :> ReqBody '[JSON] Brew :> Post '[JSON] (Maybe Int64)
    :<|> "brew" :> Capture "id" Int64 :> Get '[JSON] (Maybe Brew)

