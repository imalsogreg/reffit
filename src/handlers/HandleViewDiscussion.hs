{-# LANGUAGE OverloadedStrings #-}

module HandleViewDiscussion (handleViewDiscussion
                            , handleAddDiscussion
                            , handleDiscussion)
       where

import Reffit.Types
import Reffit.User
import Reffit.Discussion
import Reffit.Document
import Reffit.OverviewComment
import Reffit.AcidTypes
import Reffit.Sort

import Snap.Core
import Snap.Snaplet (Handler)
import Snap.Snaplet.Auth
import Application
import Snap.Snaplet.AcidState (query, update)
import Control.Lens
import Data.Time
import Control.Monad (join)
import Control.Monad.Trans (liftIO)
import qualified Data.Map as Map
import Control.Applicative ((<$>),(<*>),pure)
import Data.Text.Encoding (decodeUtf8)
import Safe
import qualified Data.Text as T
import Heist
import Snap.Snaplet.Heist
import Heist.Interpreted
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Tree as Tree
--import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS
import Data.Serialize
import Text.Digestive
import Text.Digestive.Snap
import Text.Digestive.Heist

handleDiscussion :: Handler App (AuthManager App) ()
handleDiscussion = do
  (view,result) <- runForm "form" discussionPointForm
  case result of
    Just disc -> handleAddDiscussion disc
    Nothing   -> heistLocal (bindDigestiveSplices view) $ handleViewDiscussion

-- Handle adding a discussion point to a document or an overview comment
-- Discussion point can be top-level (no dParentId param), or not
handleAddDiscussion :: Handler App (AuthManager App) ()
handleAddDiscussion = do
  us               <- query QueryAllUsers
  docs             <- query QueryAllDocs
  docId'           <- fmap join (fmap $ readMay . BS.unpack) <$> getPostParam "docId" -- Owch
  commentId'       <- fmap join (fmap $ readMay . BS.unpack) <$> getPostParam "commentId" -- Owch
  dParentId'       <- fmap join (fmap $ readMay . BS.unpack) <$> getPostParam "dParentId"
  discussionPoint' <- fmap decode <$> getPostParam "dpText" -- fmap fmap owch
  let ci = commentId'       :: Maybe OverviewCommentId
      dp = discussionPoint' :: Maybe (Either String DiscussionPoint)
  case discussionPoint' of
    Just (Right discussionPoint) -> do
      case join $ flip Map.lookup docs <$> docId' of
        Nothing -> writeText "Document not in database, or couldn't parse docid from url"
        Just doc -> case commentId' of
          Nothing->
            update $ AddDocumentDiscussionPoint discussionPoint dParentId' doc
          Just commentId ->
            case Map.lookup commentId (docOComments doc) of
              Nothing -> writeText "Didn't find that comment in the database"
              Just comment -> 
                update $ AddCommentDiscussionPoint discussionPoint dParentId' doc commentId comment
    Just (Left e) ->
      writeText $ T.append "add discussion failure: " $ T.pack e
    Nothing ->
      writeBS $ BS.unwords ["docID: ", BS.pack . show $ docId'
                           , "  commentId'", BS.pack . show $ commentId']

discussionPointForm :: Monad m => User -> DocumentId -> Maybe OverviewCommentId ->
                       Maybe DiscussionPointId ->
                       UTCTime -> Form T.Text m DiscussionPoint
discussionPointForm u docId ocId' parentId' tNow =
  DiscussionPoint 0
  <$> "dPoster" .: choice [(Just $ userName u, userName u),(Nothing,"Anonymous")] Nothing
  <*> "dText"   .: text Nothing
  <*> pure []
  <*> pure (docId, ocId', parentId')
  <*> pure tNow

handleViewDiscussion :: Handler App (AuthManager App) ()
handleViewDiscussion = do
  us    <- query QueryAllUsers
  docs  <- query QueryAllDocs
  pId'  <- getQueryParam "paperid"   --TODO: Correct?
  ocId' <- getQueryParam "commentid" --TODO: Correct?
  aUser <- currentUser
  tNow  <- liftIO $ getCurrentTime
  let u = join $ (flip Map.lookup) us <$> userLogin <$> aUser :: Maybe User
  case join $ readMay. T.unpack . decodeUtf8 <$> pId' of
    Nothing -> writeText "Need paperid parameter"
    Just pId -> do
      let commentId'  = join $ readMay . T.unpack . decodeUtf8 <$> ocId'
          doc'        = Map.lookup pId docs
          comments'   = docOComments <$> doc'
          comment'    = join $ Map.lookup <$> commentId' <*> comments'
          discussion' = maybe (docDiscussion <$> doc') (Just . ocDiscussion) comment'
      case (doc', discussion') of
        (Just doc, Just discussion) -> 
          renderWithSplices "discussion" (allDiscussionSplices doc comment' tNow discussion)

allDiscussionSplices :: Document -> Maybe OverviewComment -> UTCTime -> Discussion ->
                        Splices (SnapletISplice App)
allDiscussionSplices doc comment' tNow disc = do
  "discussionReNode" ## textSplice "TEST RE:"
  "discussionNodes"  ## (bindDiscussionPoints tNow disc)

bindDiscussionPoints :: UTCTime -> Discussion -> SnapletISplice App
bindDiscussionPoints tNow discs =
  mapSplices (callTemplate "discussion_point" . discussionTreeSplices tNow) discs

discussionTreeSplices :: UTCTime -> Tree.Tree DiscussionPoint -> Splices (SnapletISplice App)
discussionTreeSplices tNow (Tree.Node dp subs) = do
  discussionPointSplices tNow dp
  "subDiscussions" ## bindDiscussionPoints tNow subs

discussionPointSplices :: UTCTime -> DiscussionPoint -> Splices (SnapletISplice App)
discussionPointSplices tNow dp = do
  "dpAuthor" ## textSplice (maybe "Anonymous" id $ _dPoster dp)
  "dpText"   ## textSplice $ _dText dp
  "dpTime"   ## textSplice . T.pack $ sayTimeDiff tNow ( _dPostTime dp )
  
