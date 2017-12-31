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

import           Api                          (Api)
import           Types.App
import           Models                              
import           Grind                              
import           Rating                              
import           Server                       (server, withPool)

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
import           Servant hiding (GET, POST)
import           Snap.Core
import           Snap.CORS
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.Persistent
                                              --(initPersistAuthManager')
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Http.Server             (defaultConfig)
import           Servant.Server               (serveSnap, Server) 
import           System.Environment           (lookupEnv)

handleIndex :: Handler App (AuthManager App) ()
handleIndex = render "index"

--handleLogin authError = heistLocal (I.bindSplices errs) $ render "login"
handleLogin :: Maybe Text
            -> Handler App (AuthManager App) ()
handleLogin authError = renderWithSplices "login" errs
  where
    errs = maybe mempty splice authError
    splice err = "loginError" ## I.textSplice err

handleLoginSubmit :: Handler App (AuthManager App) ()
handleLoginSubmit = do
    -- TODO: try to understand precendence using this example:
    --      isLoggedIn >>= flip when $ redirect "/recent"
    --    vs.
    --      isLoggedIn >>= flip when (redirect "/recent")
    isLoggedIn >>= flip when (redirect "/recent")
    loginUser "login" "password" Nothing
                        (\err -> case err of
                                    UsernameMissing -> handleLogin Nothing
                                    _               -> handleLogin $ Just $ T.pack ("Login failed: " ++ show err))
                        (redirect "/recent") -- toS?

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
                         (redirect "/recent")

--recentBrewsHandler :: Handler App (AuthManager App) ()
recentBrewsHandler :: Handler App App ()
recentBrewsHandler = with auth currentUser >>= \case
    --isLoggedIn >>= flip unless (redirect "/login")
      Nothing   -> redirect "/login"
      Just user -> do
        let uid = unUid . fromJust . userId $ user -- TODO: fromJust
        pool <- asks (_appStateDb . _appState)
        brews <- withPool pool
            $ E.select -- TODO: order by time created
            $ E.from $ \(brew `E.InnerJoin` bean) -> do
                 E.where_ (brew ^. BrewUserIdent ==. (E.val uid))
                 E.on $ brew ^. BrewBeanId ==. bean ^. BeanId
                 return (bean, brew)
        let brews' = map (\(bean, brew) -> (entityVal bean, entityVal brew)) brews
        renderWithSplices "recent_brews" $ "recentBrews" ## renderBrews brews'
        return ()
  where
    renderBrews :: [(Bean, Brew)]
                -> SnapletISplice App
    renderBrews = I.mapSplices $ I.runChildrenWith . splicesFromBB
    splicesFromBB :: Monad n
                  => (Bean, Brew)
                  -> Splices (I.Splice n)
    splicesFromBB (Bean name roaster, Brew _ _ grind dose time yield temp rating notes) = do
        "name"          ## I.textSplice . T.pack $ name
        "roaster"       ## I.textSplice . T.pack $ roaster
        "grind"         ## I.textSplice . T.pack . show $ grind
        "dose"          ## I.textSplice . T.pack . show $ dose
        "time"          ## I.textSplice . T.pack . show $ time
        "yield"         ## I.textSplice . T.pack . show $ yield
        "temperature"   ## I.textSplice . T.pack . show $ temp
        "rating"        ## I.textSplice . T.pack . show $ rating
        "notes"         ## I.textSplice notes

newBrewHandler :: Handler App App ()
newBrewHandler = do
    beans <- allBeans
    user <- with auth currentUser
    (view, result) <- runForm "form" $ newBrewForm beans $ fromJust user
    case result of 
        Just brew -> do
            pool <- asks (_appStateDb . _appState)
            withPool pool $ insert_ brew
            --heistLocal (bindBrew brew) $ render "brew" -- does this display the entered brew?
            redirect "/recent"
        Nothing   -> heistLocal (bindDigestiveSplices view) $ render "new_brew"
  where

    -- Brew _ _ grind dose time yield temp rating notes
newBrewForm :: Monad m
            => [Entity Bean]
            -> AuthUser
            -> Form Text m Brew
newBrewForm beans user = check "Not a valid brew" validBrew $ Brew
--    <$> stringRead "Not a valid beanId = Key Bean" (Just $ BeanKey $ SqlBackendKey $ abs 42)
    <$> "beanid"      .: choice (beanChoices beans) Nothing
    <*> text (Just $ unUid . fromJust . userId $ user)
    <*> "grind"       .: choice grindChoices (Just Normal)
    <*> "dose"        .: stringRead "Not a number" (Just 11)
    <*> "time"        .: stringRead "Not a number" (Just 11)
    <*> "yield"       .: stringRead "Not a number" (Just 11)
    <*> "temperature" .: stringRead "Not a number" (Just 11)
    <*> "rating"      .: choice ratingChoices (Just Average)
    <*> "notes"       .: text (Just "")
  where
    validBrew _ = True
    grindChoices =
        [ (VeryFine,   "Very Fine"  )
        , (Fine,       "Fine"       )
        , (Normal,     "Normal"     )
        , (Coarse,     "Coarse"     )
        , (VeryCoarse, "Very Coarse")
        ]
    ratingChoices =
        [ (Terrible, "Terrible" )
        , (VeryBad,  "VeryBad"  )
        , (Bad,      "Bad"      )
        , (Average,  "Average"  )
        , (Good,     "Good"     )
        , (VeryGood, "VeryGood" )
        , (Great,    "Great"    )
        ]
    beanChoices :: [Entity Bean] -> [(Key Bean, Text)]
    beanChoices = map f
    f be = case (entityVal be) of
                Bean name roaster -> (entityKey be, T.pack $ name ++ " (" ++ roaster ++ ")")
          
allBeans = do
    pool <- asks (_appStateDb . _appState)
    beans <- withPool pool
        $ E.select -- TODO: order by time created
        $ E.from $ \bean -> do
             return bean
    return beans

newBeanHandler :: Handler App App ()
newBeanHandler = do
    (view, result) <- runForm "form" newBeanForm
    case result of 
        Just bean -> do
            pool <- asks (_appStateDb . _appState)
            withPool pool $ insert_ bean
            redirect "/new"
        Nothing   -> heistLocal (bindDigestiveSplices view) $ render "new_bean"
  where

    -- Brew _ _ grind dose time yield temp rating notes
newBeanForm :: Monad m
            => Form Text m Bean
newBeanForm = check "Not a valid bean" (const True) $ Bean
    <$> "name"    .: string Nothing
    <*> "roaster" .: string Nothing -- TODO: change to Text in bean model

initApp :: ConnectionPool
        -> SnapletInit App App
initApp pool = makeSnaplet "dial-in" "Dial in your espresso shots faster!" Nothing $ do
    s <- nestSnaplet "sess" sess $ initCookieSessionManager "site_key.txt" "sess" Nothing (Just 3600)
    h <- nestSnaplet "heist" heist $ heistInit "templates"
    _ <- liftIO $ runNoLoggingT $ runSqlPool (runMigrationUnsafe migrateAuth) pool
    _ <- liftIO $ runNoLoggingT $ runSqlPool (runMigration migrateAll) pool
    _ <- liftIO $ runNoLoggingT $ runSqlPool (initData) pool
    a <- nestSnaplet "auth" auth $ initPersistAuthManager' defAuthSettings sess pool
    addRoutes
        [ ("index",       with auth handleIndex )
        --, ("login",       with auth $ handleLogin Nothing )
        , ("login",       with auth handleLoginSubmit )
        , ("logout",      with auth handleLogout )
        , ("register",    with auth handleRegister )
        , ("recent",      recentBrewsHandler )
        , ("new",         newBrewHandler )
        , ("new_bean",    newBeanHandler )
        , ("api",         applyCORS defaultOptions $ serveSnap api server)
        , ("",            serveDirectory "static" )
        , ("/",           redirect "/recent" )
        ]
    addAuthSplices h auth
    return $ App h s a $ AppState pool
  where
    api :: Proxy (Api (AppHandler ()))
    api = Proxy
    initData = do
        --deleteWhere ([] :: [Filter Brew])
        --deleteWhere ([] :: [Filter Bean])
        --h  <- insert $ Bean "Hunkute" "Tim Wendelboe"
        --k  <- insert $ Bean "Karogoto" "Tim Wendelboe"
        --km <- insert $ Bean "Kayon Mountain" "The Barn"
        --insert_ $ Brew h (T.pack "noUserIdent") Fine 22 27 45 93 Good
        --    (T.pack "creamy, sweet. slightly bitter")
        --insert_ $ Brew k (T.pack "noUserIdent") Normal 21 31 41 94 VeryGood
        --    (T.pack "sweet syrupy, slightly underextracted.")
        return ()

--    beanId BeanId
--    userIdent Text
--    grind Grind
--    dose Int
--    time Int
--    yield Int
--    temperature Int
--    rating Rating
--    notes Text -- Text?

site :: IO ()
site = do
    --runSqlPool  $ runMigration migrateAll -- TODO
    connStr' <- lookupEnv "DATABASE_URL"
    let connStr = BS.pack $ fromMaybe localConnStr connStr'
    pool <- runStderrLoggingT $ createPostgresqlPool connStr 3
    -- ^ TODO: move this to initApp? understand whether/how to use liftBaseWith etc.
    serveSnaplet defaultConfig $ initApp pool
  where
    localConnStr = "host=localhost dbname=dialin user=dialin password=dialin"
