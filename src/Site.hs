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

import           Snap.Snaplet.AcidState (update,query,acidInit)

import           Data.Monoid
import           Control.Applicative
import           Control.Lens
import           Control.Monad.State
import           Control.Concurrent (forkIO)
import qualified Data.Acid.Remote as Acid
import           Data.Aeson
import qualified Data.Map as Map
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8)
import           Data.Time
import qualified Network as Network
import qualified Network.HTTP.Client.TLS as C
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.JsonFile
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session
import           Snap.Snaplet.AcidState
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
------------------------------------------------------------------------------
import           Application
import           Reffit.Types
import           Reffit.AcidTypes
import           Reffit.OverviewComment
import           Reffit.Document
import           Reffit.Discussion
import           Reffit.User hiding (userEmail)
import           Reffit.FieldTag
import           Reffit.CrossRef
import           Reffit.PaperRoll
import           Reffit.Handlers
import           Util.ReffitMigrate
import           Util.Mailgun

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
          am <- get
          unameMap <- query QueryAllUsers
          case Map.lookup uname unameMap of
            Nothing -> do
              _ <- registerUser "login" "password"
              _ <- update $ AddUser uname email t
              u <- loginByUsername uname (ClearText pw) True --TODO remember by default
              case u of
                  Left _ -> return ()
                  Right u' ->
                      saveUser (u' {userEmail = e}) >> return ()
              redirect "/"
            Just _ -> do
              writeText "Username is taken" -- TODO - give a helpful error message: uname is taken


------------------------------------------------------------------------------
handleDumpState :: Handler App (AuthManager App) ()
handleDumpState = do
  d <- query QueryAllDocs
  u <- query QueryAllUsers
  dc <- query QueryAllDocClasses
  ft <- query QueryAllFieldTags
  aUser <- currentUser
  case userLogin <$> aUser of
    Just "imalsogreg" ->
      writeBS . BSL.toStrict . encode $ PersistentState d u dc ft
    Just n ->
      writeText $ T.append n ", only the admin can dump_state."
    Nothing ->
      writeText $ "Only admin can dump_state."


------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes =
  [ ("login"                           , with auth handleLoginSubmit)
  , ("logout"                          , with auth handleLogout)
  , ("reset"                           , handleRequestReset)
  , ("reset/:token"                    , handleExecuteReset)
  , ("doi"                        , handleDoiSearch)
  , ("new_user"                        , with auth handleNewUser)
  , ("search"                          , with auth  handleIndex)
  , ("new_article"                     , with auth handleNewArticle)
  , ("delete_article"                  , with auth handleDeleteArticle)
  , ("new_article/:doi"                , with auth handleNewArticle)
  , ("new_summary/"                    , with auth (handleNewOComment Summary'))
  , ("edit_summary/"                   , with auth (handleNewOComment Summary'))
  , ("new_praise/"                     , with auth (handleNewOComment Praise))
  , ("new_criticism/"                  , with auth (handleNewOComment Criticism))
  , ("view_article/"                   , with auth handleViewPaper)
  , ("cast_ocomment_upvote/:idParam"   , with auth (handleOCommentVote  UpVote))
  , ("cast_ocomment_downvote/:idParam" , with auth (handleOCommentVote  DownVote))
  , ("view_discussion", with auth (method GET handleViewDiscussion <|>
                                   method POST handleAddDiscussion))
  , ("user/:username"                  , with auth handleViewUser)
  , ("follow/:username"                , with auth (handleFollow True))
  , ("unfollow/:username"              , with auth (handleFollow False))
  , ("pin/"                            , with auth (handlePin True))
  , ("unpin/"                          , with auth (handlePin False))
  , ("/add_usertag/:fieldtag"          , with auth (handleAddTag True))
  , ("/delete_usertag/:fieldtag"       , with auth (handleAddTag False))
  , ("/about"                          , render "about")
  , ("/:params"                        , with auth handleIndex)
  , ("stateToDisk"                     , with auth handleStateToDisk)
  , ("stateFromDisk"                   , with auth handleStateFromDisk)
  , ("checkpoint"                      , with auth handleCheckpoint)
  , ("/"                               , with auth handleIndex)
  , ("/dump_state"                     , with auth handleDumpState)
  , ("/transfer_emails"                , with auth setAuthUserEmails)
  , ("/splashscreen"                   , render "splashscreen")
  , ("/static"                         , serveDirectory "static")
  ]

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do

    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" Nothing (Just 3600)

    a <- nestSnaplet "auth" auth $
           initJsonFileAuthManager defAuthSettings sess "users.json"

    ac <- nestSnaplet "acid" acid $ acidInit defaultState

    void $ liftIO $ forkIO $
        Acid.acidServer Acid.skipAuthenticationCheck (Network.PortNumber 8888)
        (_acidStore $ ac ^. snapletValue)

    h <- nestSnaplet "" heist $ heistInit "templates"

    (sk, mgk) <- liftIO $ deriveFromFile "../signing-key.txt"
    mgr <- C.newTlsManager

    cref <- liftIO $ initializeCrossRef

    addRoutes routes
    addAuthSplices h auth

    -- Touch the session on each event to reset expiry time
    -- Not sure this is the best idea as it will generate a lot of extra traffic.
    -- Perhaps use longer session times?
    wrapSite (\site -> with sess touchSession >> site >> with sess commitSession)

    return $ App h s a ac sk mgr mgk cref

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

handleDoiSearch :: Handler App App ()
handleDoiSearch = do
    doi <- decodeUtf8 . rqPathInfo <$> getRequest
    liftIO $ putStrLn $ "path: " ++ T.unpack doi
    cr <- gets _cref
    r <- liftIO $ crossRef cr doi
    case r of
        Right v -> writeBS $ BSL.toStrict (encode v)
        Left e  -> writeText $ "Error calling crossref: " <> e
