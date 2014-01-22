{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module HandleViewDiscussion (handleViewDiscussion
                            , handleAddDiscussion
--                            , handleDiscussion
                            )
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
import GHC.Int

{-
handleDiscussion :: Handler App (AuthManager App) ()
handleDiscussion = do
  (view,result) <- runForm "form" discussionPointForm
  case result of
    Just disc -> handleAddDiscussion disc
    Nothing   -> heistLocal (bindDigestiveSplices view) $ handleViewDiscussion
-}

-- Handle adding a discussion point to a document or an overview comment
-- Discussion point can be top-level (no dParentId param), or not
handleAddDiscussion :: Handler App (AuthManager App) ()
handleAddDiscussion = do
  us              <- query QueryAllUsers
  docs            <- query QueryAllDocs
  docId'          <- fmap join (fmap $ readMay . BS.unpack) <$> getPostParam "docId" -- Owch
  posterId'       <- fmap (decodeUtf8) <$> getPostParam "posterId"
  commentId'      <- fmap join (fmap $ readMay . BS.unpack) <$> getPostParam "commentId" -- Owch
  dParentId'      <- fmap join (fmap $ readMay . BS.unpack) <$> getPostParam "parentId"
  discussionText' <- fmap (T.strip . decodeUtf8) <$> getPostParam "dpText" -- fmap fmap owch
  tNow            <- liftIO $ getCurrentTime
  case discussionText' of
    Just discussionText -> do
      case join $ flip Map.lookup docs <$> (docId' :: Maybe DocumentId) of
        Nothing -> do
          writeText "Document not in database, or couldn't parse docid from url"
          let tShow = T.pack . show
          writeText $ T.unwords ["docId", tShow (docId' :: Maybe DocumentId)
                                , " posterId", tShow (posterId' :: Maybe T.Text)
                                , " discussionText", tShow discussionText']
        Just doc -> case commentId' of
          Nothing-> do
            let discussionPoint = DiscussionPoint 0 posterId discussionText []
                                  (docId doc,Nothing,dParentId') tNow
                posterId = case posterId' of
                  Just n  -> if n == "" then Nothing else Just n
                  Nothing -> Nothing
            _ <- update $ AddDocumentDiscussionPoint discussionPoint dParentId' doc
            redirect "#"
          Just commentId ->
            case Map.lookup commentId (docOComments doc) of
              Nothing -> writeText "Didn't find that comment in the database"
              Just comment -> do
                let posterId = case posterId' of
                      Just n  -> if n == "" then Nothing else Just n
                      Nothing -> Nothing
                    discussionPoint = DiscussionPoint 0 posterId discussionText []
                                      (docId doc, commentId', dParentId') tNow

                _ <- update $ AddCommentDiscussionPoint discussionPoint dParentId' doc commentId comment
                writeText "Submitted to AddCommentDiscussionPoint"
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
          renderWithSplices "discussion" (allDiscussionSplices u doc commentId' comment' tNow discussion)

allDiscussionSplices :: Maybe User -> Document -> Maybe OverviewCommentId -> Maybe OverviewComment -> UTCTime -> Discussion ->
                        Splices (SnapletISplice App)
allDiscussionSplices user' doc commentId' comment' tNow disc = do
  "discussionReNode" ## textSplice "TEST RE:"
  "userName"         ## textSplice $ maybe "" userName user'
  "docid"            ## textSplice . T.pack . show . docId $ doc
  "commentid"        ## textSplice $ maybe "nocomment" (T.pack . show) commentId'
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
  "discussionId" ## textSplice . T.pack . show . _dID $ dp
  
