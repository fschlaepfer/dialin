{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Common.Model
    ( Coffee(..), CoffeeId(..)
    , Shot(..), ShotId(..)
    ) where

import           GHC.Generics
import           Data.Aeson
import           Data.Text          (Text)
import qualified Data.Text as T
import           Data.Int           (Int64)

data Coffee = Coffee
    { coffeeName    :: Text
    , coffeeRoaster :: Text
    } deriving (Generic, Eq, Show)

instance FromJSON Coffee
instance ToJSON Coffee

newtype CoffeeId = CoffeeId Int64
    deriving (Eq, Ord, ToJSON, FromJSON)

data Shot = Shot
    { shotDose          :: Int -- ^ in tenths of grams
    , shotYield         :: Int -- ^ in tenths of grams
    , shotTime          :: Int -- ^ in seconds
    , shotTemp          :: Int -- ^ in degrees celsius
    , shotGrind         :: Text
    , shotNotes         :: Text
    , shotAcidity       :: Int
    , shotBody          :: Int
    , shotSweetness     :: Int
    , shotAftertaste    :: Int
    , shotBitterness    :: Int
    } deriving (Generic, Eq, Show)

instance FromJSON Shot
instance ToJSON Shot

newtype ShotId = ShotId Int64
    deriving (Eq, Ord, ToJSON, FromJSON)
