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
import           Reffit.Discussion
import           Reffit.User
import           Reffit.FieldTag
import           Reffit.CrossRef
import           Reffit.PaperRoll

import           Reffit.Handlers

import           Util.ReffitMigrate
import           Control.Lens (view)
import           Snap.Snaplet.AcidState (Update, Query, Acid,
                                         HasAcid (getAcidStore),
                                         makeAcidic,
                                         update,query,acidInit)

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
routes =
  [ ("login" , with auth handleLoginSubmit)
  , ("logout"                          , with auth handleLogout)
  , ("new_user"                        , with auth handleNewUser)
  , ("search"                          , with auth  handleIndex)
  , ("new_article"                     , with auth handleNewArticle)
  , ("new_article/:doi"                , with auth handleNewArticle)
  , ("new_summary/:paperid"            , with auth (handleNewOComment Summary'))
  , ("new_praise/:paperid"             , with auth (handleNewOComment Praise))
  , ("new_criticism/:paperid"          , with auth (handleNewOComment Criticism))
  , ("view_article/:paperid"           , with auth handleViewPaper)
  , ("cast_ocomment_upvote/:idParam"   , with auth (handleOCommentVote  UpVote))
  , ("cast_ocomment_downvote/:idParam" , with auth (handleOCommentVote  DownVote))
  , ("view_discussion", with auth (method GET handleViewDiscussion <|>
                                   method POST handleAddDiscussion))
  , ("user/:username"                  , with auth handleViewUser)
  , ("follow/:username"                , with auth (handleFollow True))
  , ("unfollow/:username"              , with auth (handleFollow False))
  , ("pin/:paperid"                    , with auth (handlePin True))
  , ("unpin/:paperid"                  , with auth (handlePin False))
  , ("/add_usertag/:fieldtag"          , with auth (handleAddTag True))
  , ("/delete_usertag/:fieldtag"       , with auth (handleAddTag False))
  , ("/about"                          , render "about")
  , ("/:params"                        , with auth handleIndex)
  , ("stateToDisk"                     , with auth handleStateToDisk)
  , ("stateFromDisk"                   , with auth handleStateFromDisk)
  , ("checkpoint"                      , with auth handleCheckpoint)
  , ("/"                               , with auth handleIndex)
  , ("/dump_state"                     , with auth handleDumpState)
  , ("/splashscreen"                   , render "splashscreen")
  , ("/update_profile"                 , with auth handleUpdateProfile)
  , ("/static"                         , serveDirectory "static")
  ]

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do

    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)

    a <- nestSnaplet "auth" auth $
           initJsonFileAuthManager defAuthSettings sess "users.json"

    ac <- nestSnaplet "acid" acid $ acidInit defaultState
    h <- nestSnaplet "" heist $ heistInit "templates"

    addRoutes routes
    addAuthSplices h auth

    -- Touch the session on each event to reset expiry time
    -- Not sure this is the best idea as it will generate a lot of extra traffic.
    -- Perhaps use longer session times?
    wrapSite (\site -> with sess touchSession >> site >> with sess commitSession)

    return $ App h s a ac

defaultState :: PersistentState
defaultState =
  PersistentState (Map.fromList [(10,
                              Document (Just "Greg") 10 "Test Title" ["Greg","Ping"]
                              "reddit.com" (DocClass "Preprint") [["Biology","Neuroscience"]]
                              Map.empty t0 testDiscussion)])
  Map.empty
  defaultDocClasses
  tagHierarchy

defaultDocClasses :: [DocClass]
defaultDocClasses = map DocClass ["Paper","Preprint","Blog Post","Video","Book"]

testDate :: Integer -> UTCTime
testDate d = UTCTime (ModifiedJulianDay d) (fromIntegral (0::Int))

