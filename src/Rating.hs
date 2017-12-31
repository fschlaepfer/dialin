{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE TemplateHaskell            #-}

module Rating where

import           GHC.Generics
import           Data.Aeson
import           Database.Persist.TH 

data Rating
    = Terrible
    | VeryBad
    | Bad
    | Ok
    | Good
    | VeryGood
    | Great
    deriving (Show, Read, Eq, Generic)

derivePersistField "Rating"

instance FromJSON Rating
instance ToJSON Rating
