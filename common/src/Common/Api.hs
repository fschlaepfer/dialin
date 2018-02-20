{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Common.Api
    ( Api
    ) where

import           Common.Model   (Coffee, CoffeeId, Shot, ShotId)

import           Data.Int       (Int64)
import           Data.Text      (Text)
import           Servant.API

type Api =
        "coffee"                                     :> Get  '[JSON] [(Coffee, CoffeeId)]
   :<|> "coffee" :> ReqBody '[JSON] Coffee           :> Post '[JSON] CoffeeId
   :<|> "shot"                                       :> Get  '[JSON] [(Shot, Coffee)]
   :<|> "shot"   :> ReqBody '[JSON] (Shot, CoffeeId) :> Post '[JSON] ShotId
