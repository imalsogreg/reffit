{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------

import           Reffit.Types
import           Reffit.AcidTypes

import           Control.Lens (view)
import           Snap.Snaplet.AcidState (Update, Query, Acid,
                                         HasAcid (getAcidStore),
                                         makeAcidic,
                                         update,query,acidInit)

import           PaperRoll
import           HandleIndex


import           Control.Applicative
import           Data.ByteString (ByteString)
import qualified Data.Text as T
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.JsonFile
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Snaplet.AcidState
import           Snap.Util.FileServe
import           Heist
import qualified Heist.Interpreted as I
------------------------------------------------------------------------------
import           Application


------------------------------------------------------------------------------
-- | Render login form
handleLogin :: Maybe T.Text -> Handler App (AuthManager App) ()
handleLogin authError = heistLocal (I.bindSplices errs) $ render "login"
  where
    errs = maybe noSplices splice authError
    splice err = "loginError" ## I.textSplice err


------------------------------------------------------------------------------
-- | Handle login submit
handleLoginSubmit :: Handler App (AuthManager App) ()
handleLoginSubmit =
    loginUser "login" "password" Nothing
              (\_ -> handleLogin err) (redirect "/")
  where
    err = Just "Unknown user or password"


------------------------------------------------------------------------------
-- | Logs out and redirects the user to the site index.
handleLogout :: Handler App (AuthManager App) ()
handleLogout = logout >> redirect "/"



------------------------------------------------------------------------------
-- | Handle new user form submit
handleNewUser :: Handler App (AuthManager App) ()
handleNewUser = method GET handleForm <|> method POST handleFormSubmit
  where
    handleForm = render "new_user"
    handleFormSubmit = registerUser "login" "password" >> redirect "/"

------------------------------------------------------------------------------
-- | Handles article submission
handleNewArticle :: Handler App (AuthManager App) ()
handleNewArticle = update (AddDocument "TestTitle" "TestLink")


------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("/login",         with auth handleLoginSubmit)
         , ("/logout",        with auth handleLogout)
         , ("/new_user",      with auth handleNewUser)
         , ("/new_article",   with auth handleNewArticle)
         , ("/dump_articles", writeText . T.pack . show =<< query QueryAllDocs)
         , ("/test", writeText "test")
         , ("/paper_roll", handlePaperRoll)
         , ("/a_test"    , handleIndex)
--         , ("/",  handlePaperRoll)
         , ("", handleIndex)   
         ]


------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do

    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)

    -- NOTE: We're using initJsonFileAuthManager here because it's easy and
    -- doesn't require any kind of database server to run.  In practice,
    -- you'll probably want to change this to a more robust auth backend.
    a <- nestSnaplet "auth" auth $
           initJsonFileAuthManager defAuthSettings sess "users.json"

    ac <- nestSnaplet "acid" acid $ acidInit (PersistentState [] [])
    h <- nestSnaplet "" heist $ heistInit "templates"           
    addRoutes routes
    addAuthSplices h auth
    return $ App h s a ac

