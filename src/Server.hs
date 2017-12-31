{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Server
    ( server
    , withPool
    ) where

import           Models
import           Api
import           Types.App

import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Logger         (runStderrLoggingT, NoLoggingT, runNoLoggingT)
import           Control.Monad.Trans.Resource
import           Control.Monad.Reader
import           Data.Int                     (Int64)
import           Data.Text                    (Text)
import qualified Data.Text as T
import           Database.Persist
import           Database.Persist.Sql         --(runSqlPool)
import           Database.Persist.Postgresql
import           Servant
import           Servant.Server               (Server)

withPool :: MonadIO m
         => ConnectionPool
         -> SqlPersistT (ResourceT (NoLoggingT IO)) a
         -> m a
withPool pool query = liftIO . runNoLoggingT . runResourceT $ runSqlPool query pool

server :: Server (Api (AppHandler ())) '[] AppHandler
server = createBean
    :<|> getBean
    :<|> createBrew
    :<|> getBrew
  where
    --createPerson :: Person -> AppHandler (Maybe (Key Person))
    --createPerson (Person name) = do
    --    pool <- asks (_appStateDb . _appState)
    --    exists <- withPool pool $ selectFirst [PersonName ==. name] []
    --    case exists of
    --        Nothing -> Just <$> withPool pool (insert $ Person name)
    --        Just _  -> return Nothing

    --getPerson :: Text -> AppHandler (Maybe Person)
    --getPerson name = do
    --    pool <- asks (_appStateDb . _appState)
    --    person <- withPool pool $ selectFirst [PersonName ==. T.unpack name] []
    --    return $ entityVal <$> person

    createBean :: Bean -> AppHandler (Maybe Int64)
    createBean = undefined

    getBean :: Int64 -> AppHandler (Maybe Bean)
    getBean = undefined

    createBrew :: Brew -> AppHandler (Maybe Int64)
    createBrew = undefined

    getBrew :: Int64 -> AppHandler (Maybe Brew)
    getBrew = undefined

