{-# LANGUAGE DeriveGeneric #-}

module Common.Foo where

import           Data.Aeson
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics

data Foo = Foo
    { fooName  :: Text
    , fooValue :: Integer
    } deriving Generic

instance FromJSON Foo
instance ToJSON Foo
