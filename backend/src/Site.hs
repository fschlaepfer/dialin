{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE LambdaCase                 #-}

module Site
    ( site
    ) where

import           Common.Api                   (Api)
import           Common.Model                 (Coffee(..), CoffeeId(..), Shot(..), ShotId(..))
import           Types.App
import           Models                              
import           Server                       (withPool)

import           Control.Applicative
import           Control.Lens                 (makeLenses)
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Logger         (runStderrLoggingT, NoLoggingT, runNoLoggingT)
import           Control.Monad.Trans.Resource 
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Map.Syntax              ((##))
import           Data.Maybe                   (maybe, fromJust, fromMaybe)
import           Data.Text                    (Text)
import qualified Data.Text as T
import           Data.Pool                    
import qualified Data.ByteString.Char8 as BS
import           Database.Persist             hiding ((==.))
import           Database.Persist.Sql         hiding ((==.))
import           Database.Persist.Postgresql  hiding ((==.))
import           Database.Persist.TH 
import qualified Database.Esqueleto as E
import           Database.Esqueleto           ((^.), (==.))
import           Text.Digestive
import           Text.Digestive.Heist
import           Text.Digestive.Snap          hiding (method)
import           Heist
import qualified Heist.Interpreted as I
import           Servant                      hiding (GET, POST)
import           Snap.Core
import           Snap.CORS
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.Persistent
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Http.Server             (defaultConfig)
import           Servant.Server               (serveSnap, Server) 
import           System.Environment           (lookupEnv)

handleIndex :: Handler App App ()
handleIndex = render "index"

handleLogin :: Maybe Text -> Handler App (AuthManager App) ()
handleLogin authError = renderWithSplices "login" errs
  where
    errs = maybe mempty splice authError
    splice err = "loginError" ## I.textSplice err

handleLoginSubmit :: Handler App (AuthManager App) ()
handleLoginSubmit = do
    isLoggedIn >>= flip when (redirect "/index")
    loginUser "login" "password" Nothing
                        (\err -> case err of
                                    UsernameMissing -> handleLogin Nothing
                                    _               -> handleLogin $ Just $ T.pack ("Login failed: " ++ show err))
                        (redirect "/index")

handleLogout :: Handler App (AuthManager App) ()
handleLogout = logout >> redirect "/login"

handleRegister :: Handler App (AuthManager App) ()
handleRegister = method GET handleForm <|> method POST handleFormSubmit
  where
    handleForm = render "register"
    handleFormSubmit = do
        res <- registerUser "login" "password"
        case res of
            Left e  -> writeText $ T.pack $ "Error creating user: " ++ show e
            Right _ -> loginUser "login" "password" Nothing -- This is currently identical to handleLoginSubmit. Adjust for register differences. Extend register template with error message, just like login.
                         (\err -> handleLogin $ Just $ T.pack ("Login failed: " ++ show err))
                         (redirect "/index")

server :: Server Api '[] AppHandler
server = getCoffees :<|> newCoffee :<|> getShots :<|> newShot
  where
    getCoffees :: (Handler App App) [(Coffee, CoffeeId)]
    getCoffees = do
        pool <- asks (_appStateDb . _appState)
        beans <- withPool pool $ selectList [] []
        return $ map beanToCoffee beans
    beanToCoffee :: Entity Bean -> (Coffee, CoffeeId)
    beanToCoffee (Entity key (Bean name roaster))
        = (Coffee name roaster, CoffeeId $ fromSqlKey key)

    newCoffee :: Coffee -> (Handler App App) CoffeeId
    newCoffee c@(Coffee name roaster) = do
        pool <- asks (_appStateDb . _appState)
        (CoffeeId . fromSqlKey) <$> (withPool pool $ insert $ Bean name roaster)

    getShots :: (Handler App App) [(Shot, Coffee)]
    getShots = do
        pool <- asks (_appStateDb . _appState)
        -- TODO: 403 if not logged in.
        u <- with auth currentUser
        let uid = unUid . fromJust . userId $ fromJust u
        brews' <- withPool pool
            $ E.select
            $ E.from $ \(brew `E.InnerJoin` bean) -> do
                 E.where_ (brew ^. BrewUserIdent ==. (E.val uid))
                 E.on $ brew ^. BrewBeanId ==. bean ^. BeanId
                 return (bean, brew)
        return $ map toShot $ map (\(bean, brew) -> (entityVal bean, entityVal brew)) brews'
    toShot :: (Bean, Brew) -> (Shot, Coffee)
    toShot (Bean name roaster, Brew beanId _ dose yield time temp grind notes acidity body sweetness aftertaste bitterness)
        = (Shot dose yield time temp grind notes acidity body sweetness aftertaste bitterness, Coffee name roaster)
        
    newShot :: (Shot, CoffeeId) -> (Handler App App) ShotId
    newShot (s@(Shot dose yield time temp grind notes acidity body sweetness aftertaste bitterness),
            CoffeeId beanId) = do
        pool <- asks (_appStateDb . _appState)
        user <- with auth currentUser
        let beanKey :: Key Bean = toSqlKey beanId
        
        --let userIdent = "localTestUser"
        let userIdent = unUid . fromJust . userId $ fromJust user

        (ShotId . fromSqlKey) <$> (withPool pool $ insert $
            Brew beanKey userIdent dose yield time temp grind notes acidity body sweetness aftertaste bitterness)

initApp :: ConnectionPool -> SnapletInit App App
initApp pool = makeSnaplet "dial-in" "Dial in your espresso shots faster!" Nothing $ do
    s <- nestSnaplet "sess" sess $ initCookieSessionManager "site_key.txt" "sess" Nothing (Just 3600)
    h <- nestSnaplet "heist" heist $ heistInit "templates"
    _ <- liftIO $ runNoLoggingT $ runSqlPool (runMigrationUnsafe migrateAuth) pool
    _ <- liftIO $ runNoLoggingT $ runSqlPool (runMigration migrateAll) pool
    a <- nestSnaplet "auth" auth $ initPersistAuthManager' defAuthSettings sess pool
    addRoutes
        [ ("index",       handleIndex )
        , ("login",       with auth handleLoginSubmit )
        , ("logout",      with auth handleLogout )
        , ("register",    with auth handleRegister )
        , ("api",         applyCORS defaultOptions $ serveSnap (Proxy :: Proxy Api) server >> applyCORS defaultOptions (pure ()))
        , ("",            serveDirectory "static" )
        , ("/",           redirect "/index" )
        ]
    addAuthSplices h auth
    return $ App h s a $ AppState pool

site :: IO ()
site = do
    connStr' <- lookupEnv "DATABASE_URL"
    let connStr = BS.pack $ fromMaybe localConnStr connStr'
    pool <- runStderrLoggingT $ createPostgresqlPool connStr 3
    serveSnaplet defaultConfig $ initApp pool
  where
    localConnStr = "host=localhost dbname=dialin user=dialin password=dialin"
