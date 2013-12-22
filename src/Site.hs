{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app, testDoc -- TODO debugging
  ) where

------------------------------------------------------------------------------

import           Reffit.Types
import           Reffit.AcidTypes
import           Reffit.FieldTag
import           Reffit.CrossRef

import           Control.Lens (view)
import           Snap.Snaplet.AcidState (Update, Query, Acid,
                                         HasAcid (getAcidStore),
                                         makeAcidic,
                                         update,query,acidInit)

import           PaperRoll
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
import           GHC.Int  -- TODO for add1000 test
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
      e <- fmap decodeUtf8 <$> getParam "email"
      t <- liftIO $ getCurrentTime
      case (l,e) of
        (Nothing,_) -> redirect "/" -- TODO - Give a helpful error message
        (_,Nothing) -> redirect "/" -- TODO - Give a helpful error message
        (Just uname, Just email) -> do
          unameMap <- query QueryAllUsers
          case Map.lookup uname unameMap of
            Nothing -> do
              _ <- registerUser "login" "password"
              _ <- update $ AddUser uname email t
              redirect "/"
            Just _ -> do
              writeText "Username is taken" -- TODO - give a helpful error message: uname is taken

handleDumpState :: Handler App App ()
handleDumpState = do
  d <- query QueryAllDocs
  u <- query QueryAllUsers
  dc <- query QueryAllDocClasses
  ft <- query QueryAllFieldTags
  writeText . T.pack . show $ PersistentState d u dc ft

handleAdd1000 :: Handler App App ()
handleAdd1000 = do
  -- d <- query QueryAllDocs
  forM_ [1..(1000::Int)] $ \i -> update $ AddDocument Nothing (testDoc (fromIntegral i))

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [
      ("search",    with auth  handleIndex)
    , ("login",         with auth handleLoginSubmit)
    , ("logout",        with auth handleLogout)
    , ("new_user",      with auth handleNewUser)
    , ("new_article",        with auth handleNewArticle)
    , ("new_article/:doi",   with auth handleNewArticle)
    , ("new_summary/:paperid", with auth handleNewSummary)
    , ("new_praise/:paperid", with auth (handleNewCritique UpVote))
    , ("new_criticism/:paperid", with auth (handleNewCritique DownVote))
    , ("view_article/:paperid", with auth handleViewPaper) 
    , ("cast_summary_upvote/:idParam",    with auth $ handleSummaryVote  UpVote)
    , ("cast_summary_downvote/:idParam",  with auth $ handleSummaryVote  DownVote)
    , ("cast_critique_upvote/:idParam",  with auth $ handleCritiqueVote UpVote)
    , ("cast_critique_downvote/:idParam",with auth $ handleCritiqueVote DownVote)
    , ("user/:username", with auth $ handleViewUser)
    , ("follow/:username", with auth   $ handleFollow True)
    , ("unfollow/:username", with auth $ handleFollow False)
    , ("pin/:paperid",   with auth $ handlePin True) 
    , ("unpin/:paperid", with auth $ handlePin False)
    , ("/add_usertag/:fieldtag", with auth $ handleAddTag True)  
    , ("/delete_usertag/:fieldtag", with auth $ handleAddTag False)
    , ("/about", render "about")
    , ("/:params" , with auth $ handleIndex)
    , ("/", with auth $ handleIndex)

--    , ("add_1000",      handleAdd1000) -- TODO just testing
--    , ("paper_roll", handlePaperRoll) -- do I still need this?  I have HandleIndex    
--    , ("/dump_articles", writeText . T.pack . show =<< query QueryAllDocs)
--    , ("/dump_state", handleDumpState)
--    , ("/test", writeText "test")
--    , ("/new_doc_class", with auth handleNewDocClass)
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
convenienceReset = PersistentState Map.empty Map.empty [DocClass "Paper", DocClass "Preprint"
                                                       ,DocClass "Blog Post", DocClass "Video"
                                                       ,DocClass "Book"] testTags 

stresstestReset :: PersistentState
stresstestReset = PersistentState docs Map.empty [DocClass "Paper"] testTags
  where
    docs = Map.fromList [(i, Document Nothing i "The Earth is Round (p < .05)" []
              "https://www.ics.uci.edu/~sternh/courses/210/cohen94_pval.pdf" (DocClass "Paper") [] Map.empty Map.empty (testDate (fromIntegral i)))
           | i <- [1..1000]] 
             
testDoc :: Int32 -> Document
testDoc i = Document Nothing i "The Earth is Round (p < .05)" ["Jacob Cohen","Hans Ruthorford Jr."]
            "https://www.ics.uci.edu/~sternh/courses/210/cohen94_pval.pdf" (DocClass "Paper") [] (Map.fromList [(0,testSummary)]) (Map.fromList [(0,testPraise)]) (testDate 0)

testUsers = [ User "Arte Artimus" "arte@test.com" (Set.fromList ["Santa","Rudolph"]) Set.empty [] Set.empty ] 

testPraise = Critique "This was a really awesome paper.  High cool points" (Just "Arte Artimus") Coolness  UpVote [UpVote,DownVote,UpVote] (testDate 0)

testSummary = Summary Nothing "This paper talks about why H0 hypothesist testing isn't appropriate for describing effect size." [UpVote,UpVote] (testDate 0)

testDate :: Integer -> UTCTime
testDate d = UTCTime (ModifiedJulianDay d) (fromIntegral (0::Int)) 