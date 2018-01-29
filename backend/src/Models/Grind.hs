{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE TemplateHaskell            #-}

module Models.Grind where

import           GHC.Generics
import           Data.Aeson
import           Database.Persist.TH 

data Grind
    = VeryFine
    | Fine
    | Normal
    | Coarse
    | VeryCoarse
    deriving (Show, Read, Eq, Generic)

derivePersistField "Grind"

instance FromJSON Grind
instance ToJSON Grind
