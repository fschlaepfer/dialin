{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}

module Types.App where

import           Control.Lens                 (makeLenses)
import           Data.Pool                    
import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.Postgresql
import           Snap.Snaplet                 
import           Snap.Snaplet.Heist
import           Heist
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Session
import           Snap.Snaplet.Session.Backends.CookieSession

data AppState = AppState
    { _appStateDb :: Pool SqlBackend --ConnectionPool
    }
makeLenses ''AppState

data App = App
    { _heist    :: Snaplet (Heist App)
    , _sess     :: Snaplet SessionManager
    , _auth     :: Snaplet (AuthManager App)
    , _appState :: AppState
    }
makeLenses ''App

instance HasHeist App where
    heistLens = subSnaplet heist

type AppHandler = Handler App App
