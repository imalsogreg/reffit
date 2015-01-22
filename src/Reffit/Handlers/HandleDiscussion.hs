{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Reffit.Handlers.HandleDiscussion (
      handleViewDiscussion
    , handleAddDiscussion
    )
       where

import Control.Lens

import Application
import Reffit.Types
import Reffit.User
import Reffit.Discussion
import Reffit.Document
import Reffit.OverviewComment
import Reffit.AcidTypes
import Reffit.Sort
import Reffit.Scores
import Reffit.Markdown

import qualified Data.Tree as Tree
import qualified Data.ByteString.Char8 as BS

import Snap.Core
import Snap.Snaplet (Handler)
import Snap.Snaplet.Auth

import Snap.Snaplet.AcidState (query, update)
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

import           Text.Digestive


------------------------------------------------------------------------------
-- Handle adding a discussion point to a document or an overview comment
-- Discussion point can be top-level (no dParentId param), or not
handleAddDiscussion :: Handler App (AuthManager App) ()
handleAddDiscussion = do
  us              <- query QueryAllUsers
  docs            <- query QueryAllDocs
  docId'          <- fmap join (fmap $ readMay . BS.unpack) <$>
                     getPostParam "docId"
  posterId'       <- fmap (decodeUtf8) <$>
                     getPostParam "posterId"
  commentId'      <- fmap join (fmap $ readMay . BS.unpack) <$>
                     getPostParam "commentId"
  dParentId'      <- fmap join (fmap $ readMay . BS.unpack) <$>
                     getPostParam "parentId"
  discussionText' <- fmap (T.strip . decodeUtf8) <$>
                     getPostParam "dpText"
  tNow            <- liftIO $ getCurrentTime
  case discussionText' of
    Just discussionText -> do
      case join $ flip Map.lookup docs <$> (docId' :: Maybe DocumentId) of
        Nothing -> do
          writeText "Document not in database, or couldn't parse docid from url"
          let tShow = T.pack . show
          writeText $ T.unwords ["docId", tShow (docId' :: Maybe DocumentId)
                                ," posterId", tShow (posterId' :: Maybe T.Text)
                                ," discussionText", tShow discussionText']
        Just doc -> case commentId' of
          Nothing-> do
            let discussionPoint =
                  DiscussionPoint 0 posterId discussionText []
                  (docId doc,Nothing,dParentId') tNow
                posterId = case posterId' of
                  Just n  -> if n == "" then Nothing else Just n
                  Nothing -> Nothing
            _ <- update $
                 AddDocumentDiscussionPoint discussionPoint dParentId' doc
            redirect "#"
          Just commentId ->
            case Map.lookup commentId (docOComments doc) of
              Nothing -> writeText "Didn't find that comment in the database"
              Just comment -> do
                let posterId = case posterId' of
                      Just n  -> if n == "" then Nothing else Just n
                      Nothing -> Nothing
                    discussionPoint =
                      DiscussionPoint 0 posterId discussionText []
                      (docId doc, commentId', dParentId') tNow
                _ <- update $
                     AddCommentDiscussionPoint discussionPoint dParentId'
                     doc commentId comment
                redirect "#"
    Nothing ->
      writeBS $ BS.unwords ["docID: ", BS.pack . show $ docId'
                           , "  commentId'", BS.pack . show $ commentId']

------------------------------------------------------------------------------
discussionPointForm :: Monad m => User -> DocumentId ->
                       Maybe OverviewCommentId ->
                       Maybe DiscussionPointId ->
                       UTCTime -> Form T.Text m DiscussionPoint
discussionPointForm u docId ocId' parentId' tNow =
  DiscussionPoint 0
  <$> "dPoster" .: choice [(Just $ userName u, userName u)
                          ,(Nothing,"Anonymous")] Nothing
  <*> "dText"   .: text Nothing
  <*> pure []
  <*> pure (docId, ocId', parentId')
  <*> pure tNow


------------------------------------------------------------------------------
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
          discussion' = maybe (docDiscussion <$> doc')
                        (Just . ocDiscussion) comment'
      case (doc', discussion') of
        (Just doc, Just discussion) ->
          renderWithSplices "discussion"
            (allDiscussionSplices u us doc docs
             commentId' comment' tNow discussion)


------------------------------------------------------------------------------
allDiscussionSplices :: Maybe User -> Map.Map UserName User ->
                        Document -> Map.Map DocumentId Document ->
                        Maybe OverviewCommentId -> Maybe OverviewComment ->
                        UTCTime -> Discussion ->
                        Splices (SnapletISplice App)
allDiscussionSplices user' us doc docs commentId' comment' tNow disc = do
  let posterText   = maybe "" (\t -> T.append t "'s")
                     (join $ ocPoster <$> comment')
      discTypeText = maybe "document" (const "comment") comment'
  "userRep"          ## textSplice $
    maybe "" (T.pack . show . userReputation docs) user'
  "discussionType"   ## textSplice $ T.concat [posterText," ",discTypeText]
  "discussionReNode" ## textSplice $ maybe (docTitle doc) (ocText) comment'
  "userName"         ## textSplice $ maybe "" userName user'
  "docid"            ## textSplice . T.pack . show . docId $ doc
  "commentid"        ## textSplice $
    maybe "nocomment" (T.pack . show) commentId'
  "discussionNodes"  ## (bindDiscussionPoints docs us tNow disc)


------------------------------------------------------------------------------
bindDiscussionPoints :: Map.Map DocumentId Document ->
                        Map.Map UserName User ->
                        UTCTime -> Discussion -> SnapletISplice App
bindDiscussionPoints docs us tNow discs =
  mapSplices (callTemplate "discussion_point" .
              discussionTreeSplices docs us tNow) discs

discussionTreeSplices :: Map.Map DocumentId Document ->
                         Map.Map UserName User ->
                         UTCTime -> Tree.Tree DiscussionPoint ->
                         Splices (SnapletISplice App)
discussionTreeSplices docs us tNow (Tree.Node dp subs) = do
  discussionPointSplices docs us tNow dp
  "subDiscussions" ## bindDiscussionPoints docs us tNow subs


------------------------------------------------------------------------------
discussionPointSplices :: Map.Map DocumentId Document
                          -> Map.Map UserName User
                          -> UTCTime -> DiscussionPoint
                          -> Splices (SnapletISplice App)
discussionPointSplices docs us tNow dp = do
  "dpAuthor"     ## textSplice $ authorText
  "authorLink"   ## textSplice $ maybe "#" (T.append "/user/") (_dPoster dp)
  "dpText"       ## markdownSplice (dp^.dText)
  "dpTime"       ## textSplice . T.pack $ sayTimeDiff tNow ( _dPostTime dp )
  "discussionId" ## textSplice . T.pack . show . _dID $ dp
  where
    userRepText u = T.pack . show $ userReputation docs u
    authorText = maybe "Anonymous"
                 (\u -> T.concat [userName u,"(",userRepText u,")"])
                . join $ Map.lookup <$> (_dPoster dp) <*> pure us

