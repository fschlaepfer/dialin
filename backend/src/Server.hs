{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Server
    ( withPool
    ) where

import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Logger         (runStderrLoggingT, NoLoggingT, runNoLoggingT)
import           Control.Monad.Trans.Resource
import           Control.Monad.Reader
import qualified Data.Text as T
import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.Postgresql

withPool :: MonadIO m
         => ConnectionPool
         -> SqlPersistT (ResourceT (NoLoggingT IO)) a
         -> m a
withPool pool query = liftIO . runNoLoggingT . runResourceT $ runSqlPool query pool
