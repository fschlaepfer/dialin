{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}

module Models where

import           Models.Grind
import           Models.Rating
import           Data.Text                    (Text)
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH 

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  Bean json
    name String
    roaster String
    deriving Eq Show
  Brew json
    beanId BeanId
    userIdent Text
    grind Grind
    dose Double
    time Int
    yield Double
    temperature Int
    rating Rating
    notes Text
    deriving Eq Show
|]

