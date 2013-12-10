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
import           HandleNewPaper

import           Control.Applicative
import qualified Data.Map as Map
import           Data.ByteString (ByteString)
import qualified Data.Text as T
import           Snap (gets)
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
import qualified Text.Blaze.Html5  as H
import           Text.Digestive
import           Text.Digestive.Snap (runForm)
import           Text.Digestive.Heist  
import           Text.Digestive.Blaze.Html5

import           Control.Monad.CatchIO (throw)
import           Control.Monad.State
import           Data.Text.Encoding (decodeUtf8)
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
-- TODO - make sure user by that name doesn't already exist!
handleNewUser :: Handler App (AuthManager App) ()
handleNewUser = method GET handleForm <|> method POST handleFormSubmit
  where
    handleForm = render "new_user"
    handleFormSubmit = do
      l <- fmap decodeUtf8 <$> getParam "login"
      case l of
        Nothing    -> redirect "/" -- TODO - Give a helpful error message
        Just uname -> do
          unameMap <- query QueryAllUsers
          case Map.lookup uname unameMap of
            Nothing -> do
              _ <- registerUser "login" "password"
              _ <- update $ AddUser uname 
              redirect "/"
            Just _ -> do
              redirect "/" -- TODO - give a helpful error message: uname is taken


------------------------------------------------------------------------------
-- | Handles article submission
handleNewArticle :: Handler App (AuthManager App) ()
--handleNewArticle = method GET handleForm <|> method POST handleFormSubmit
handleNewArticle = handleForm 
  where
   handleForm = do
     userMap <- query QueryAllUsers
     authUser' <- currentUser
     case (Map.lookup <$> (userLogin <$> authUser') <*> pure userMap) of
       Nothing -> writeText "Error - authUser not in app user database"
       Just Nothing -> writeText "Error - justNothing, I'm not sure how you'd get this."
       Just (Just user)  -> do
         (vw,rs) <- runForm "new_paper_form" $ documentForm user [] []
         case rs of 
           Just doc -> do --TODO add the actual paper, not this test paper.
             _ <- update $
                  AddDocument Nothing "TestTitle" ["Greg","Andy"] "http://www.github.com" (DocClass "Paper")
--             redirect "/" -- TODO: redirect to the new page for that paper
             writeText . (T.append "Got Document: " ) . T.pack . show $ doc
           Nothing -> do
--             let nodes = renderHtmlNodes $ showForm "/" "post" form
             heistLocal (bindDigestiveSplices vw) $ render "new_paper"
--   handleFormSubmit = update
--                   (AddDocument Nothing "TestTitle" ["Greg Hale", "Andy Bolton"]
--                    "http://www.github.com" (DocClass "Paper")) >> redirect "/" -- TODO redirect to new paper

 
------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [
    ("/", handleIndex)   
  , ("/login",         with auth handleLoginSubmit)
  , ("/logout",        with auth handleLogout)
  , ("/new_user",      with auth handleNewUser)
  , ("/new_article",   with auth handleNewArticle)
  , ("/dump_articles", writeText . T.pack . show =<< query QueryAllDocs)
  , ("/test", writeText "test")
  , ("/paper_roll", handlePaperRoll)
  , ("/static", serveDirectory "static")
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

    ac <- nestSnaplet "acid" acid $ acidInit (PersistentState [] Map.empty)
    h <- nestSnaplet "" heist $ heistInit "templates"           
    addRoutes routes
    addAuthSplices h auth
    return $ App h s a ac

