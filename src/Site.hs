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
import           Reffit.OverviewComment
import           Reffit.Document
import           Reffit.User
import           Reffit.FieldTag
import           Reffit.CrossRef
import           Reffit.PaperRoll
import           Util.ReffitMigrate

import           Control.Lens (view)
import           Snap.Snaplet.AcidState (Update, Query, Acid,
                                         HasAcid (getAcidStore),
                                         makeAcidic,
                                         update,query,acidInit)


import           HandleIndex
import           HandleNewPaper
import           HandleNewDocClass
import           HandleViewPaper
import           HandleNewSummary
import           HandleNewCritique
import           HandleSummaryVote
import           HandleViewUser

import           Control.Applicative
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.ByteString (ByteString)
import qualified Data.Text as T
import           Snap (gets)
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.JsonFile
import           Snap.Snaplet.Heist 
import           Snap.Snaplet.Session
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
import           Data.Time
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
    loginUser "login" "password" (Just "remember")
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
      e <- fmap decodeUtf8 <$> getParam "email"
      p <- getParam "password"
      t <- liftIO $ getCurrentTime
      case (l,e,p) of
        (Nothing,_,_) -> redirect "/" -- TODO - Give a helpful error message
        (_,Nothing,_) -> redirect "/" -- TODO - Give a helpful error message
        (_,_,Nothing) -> redirect "/" -- TODO -- error message
        (Just uname, Just email, Just pw) -> do
          unameMap <- query QueryAllUsers
          case Map.lookup uname unameMap of
            Nothing -> do
              _ <- registerUser "login" "password"
              _ <- update $ AddUser uname email t
              _ <- loginByUsername uname (ClearText pw) True --TODO remember by default
              redirect "/" 
            Just _ -> do
              writeText "Username is taken" -- TODO - give a helpful error message: uname is taken

handleDumpState :: Handler App (AuthManager App) ()
handleDumpState = do 
  d <- query QueryAllDocs
  u <- query QueryAllUsers
  dc <- query QueryAllDocClasses
  ft <- query QueryAllFieldTags
  writeText . T.pack . show $ PersistentState d u dc ft


------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [
      ("login",         with auth handleLoginSubmit)
    , ("logout",        with auth handleLogout)
    , ("new_user",      with auth handleNewUser)
    , ("search",    
       -- TODO: Is this the right way to refresh the session on every action??
       withSession sess (with sess touchSession) >> 
       with auth  handleIndex)
    , ("new_article",   
       withSession sess (with sess touchSession) >>
       with auth handleNewArticle) 
    , ("new_article/:doi",   
       withSession sess (with sess touchSession) >> 
       with auth handleNewArticle)
    , ("new_summary/:paperid", 
       withSession sess (with sess touchSession) >>
       with auth (handleNewOComment Summary'))
    , ("new_praise/:paperid", 
       withSession sess (with sess touchSession) >> 
       with auth (handleNewOComment Praise)) 
    , ("new_criticism/:paperid", 
       withSession sess (with sess touchSession) >>
       with auth (handleNewOComment Criticism))
    , ("view_article/:paperid", 
       withSession sess (with sess touchSession) >>
       with auth handleViewPaper) 
    , ("cast_ocomment_upvote/:idParam",    
       withSession sess (with sess touchSession) >>
       with auth (handleOCommentVote  UpVote))
    , ("cast_ocomment_downvote/:idParam",  
       withSession sess (with sess touchSession) >>
       with auth (handleOCommentVote  DownVote))
    , ("user/:username", 
       withSession sess (with sess touchSession) >>
       with auth handleViewUser)
    , ("follow/:username", 
       withSession sess (with sess touchSession) >>
       with auth (handleFollow True))
    , ("unfollow/:username", 
       withSession sess (with sess touchSession) >>
       with auth (handleFollow False))
    , ("pin/:paperid",   
       withSession sess (with sess touchSession) >>
       with auth (handlePin True)) 
    , ("unpin/:paperid", 
       withSession sess (with sess touchSession) >>
       with auth (handlePin False))
    , ("/add_usertag/:fieldtag", 
       withSession sess (with sess touchSession) >>
       with auth (handleAddTag True))  
    , ("/delete_usertag/:fieldtag",
       withSession sess (with sess touchSession) >>
       with auth (handleAddTag False))
    , ("/about",
       withSession sess (with sess touchSession) >>
       render "about")
    , ("/:params" ,
       withSession sess (with sess touchSession) >>
       with auth handleIndex)
    
    , ("stateToDisk",   with auth handleStateToDisk)
    , ("stateFromDisk", with auth handleStateFromDisk)
    , ("checkpoint",    with auth handleCheckpoint)
--    , ("migrateStateFromDisk", with auth handleMigrateStateFromDisk)
      
    , ("/",
       withSession sess (with sess touchSession) >>
       with auth handleIndex)

    , ("/dump_state",  with auth handleDumpState)
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

    ac <- nestSnaplet "acid" acid $ acidInit convenienceReset
    h <- nestSnaplet "" heist $ heistInit "templates"           
    addRoutes routes
    addAuthSplices h auth
    return $ App h s a ac


factoryReset :: PersistentState
factoryReset = PersistentState Map.empty Map.empty [] []

convenienceReset :: PersistentState
convenienceReset = PersistentState Map.empty Map.empty [] tagHierarchy

testDate :: Integer -> UTCTime
testDate d = UTCTime (ModifiedJulianDay d) (fromIntegral (0::Int)) 