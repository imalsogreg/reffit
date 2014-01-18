{-# LANGUAGE OverloadedStrings #-}

module HandleViewDiscussion (handleViewDiscussion) where

import Reffit.Types
import Reffit.User
import Reffit.Discussion
import Reffit.Document
import Reffit.OverviewComment
import Reffit.AcidTypes

import Snap.Core
import Snap.Snaplet (Handler)
import Snap.Snaplet.Auth
import Application
import Snap.Snaplet.AcidState (query)
import Control.Lens
import Data.Time
import Control.Monad (join)
import Control.Monad.Trans (liftIO)
import qualified Data.Map as Map
import Control.Applicative ((<$>),(<*>))
import Data.Text.Encoding (decodeUtf8)
import Safe
import qualified Data.Text as T
import Heist
import Snap.Snaplet.Heist
import Heist.Interpreted

handleViewDiscussion :: Handler App (AuthManager App) ()
handleViewDiscussion = do
  us    <- query QueryAllUsers
  docs  <- query QueryAllDocs
  pId'  <- getParam "paperid"
  ocId' <- getParam "commentid"
  aUser <- currentUser
  tNow  <- liftIO $ getCurrentTime
  let u = join $ (flip Map.lookup) us <$> userLogin <$> aUser :: Maybe User
  case join $ readMay. T.unpack . decodeUtf8 <$> pId' of
    Nothing -> writeText "Need paperid parameter"
    Just pId -> do
      let commentId' = join $ readMay . T.unpack . decodeUtf8 <$> ocId' :: Maybe OverviewCommentId
          doc'       = Map.lookup pId docs
          comments'  = docOComments <$> doc' :: Maybe (Map.Map OverviewCommentId OverviewComment)
          comment'   = join $ Map.lookup <$> commentId' <*> comments'
      case doc' of
        Nothing -> writeText "Couldn't find document in database"
        Just doc -> 
          renderWithSplices "discussion" (allDiscussionSplices doc comment' tNow)

allDiscussionSplices :: Document -> Maybe OverviewComment -> UTCTime -> Splices (SnapletISplice App)
allDiscussionSplices doc comment' tNow = do
  "discussionReNode" ## textSplice "TEST RE:"
      

discussionPointSplices :: DiscussionPoint -> Splices (SnapletISplice App)
discussionPointSplices dp = do
  "dpAuthor" ## textSplice (maybe "Anonymous" id $ _dPoster dp)
