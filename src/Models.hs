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

import           Grind
import           Rating
import           Data.Text                    (Text)
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH 

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
--  Person json
--    name String
  Bean json
    name String
    roaster String
    deriving Eq Show
  Brew json
    beanId BeanId
    userIdent Text
    grind Grind
    dose Int
    time Int
    yield Int
    temperature Int
    rating Rating
    notes Text -- Text?
    deriving Eq Show
|]

