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

import           Data.Text                    (Text)
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH 

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  Bean json
    name    Text
    roaster Text
    deriving Eq Show

  Brew json
    beanId      BeanId
    userIdent   Text
    dose        Int
    time        Int
    yield       Int
    temperature Int
    grind       Text
    notes       Text
    acidity     Int
    body        Int
    sweetness   Int
    aftertaste  Int
    bitterness  Int
    deriving Eq Show
|]

