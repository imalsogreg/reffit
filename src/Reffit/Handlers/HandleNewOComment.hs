{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Reffit.Handlers.HandleNewOComment(
  newOCommentForm,
  handleNewOComment
  )
where

import           Reffit.Types
import           Reffit.AcidTypes
import           Reffit.Document
import           Reffit.OverviewComment
import           Reffit.User

import           Safe
import           Application
import           Snap.Snaplet.AcidState (update,query)
import           Snap.Core
import           Snap.Snaplet(Handler)
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Auth
import           Control.Applicative
import           Control.Monad.Trans
import qualified Data.Map                     as Map
import           Data.Map.Syntax
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Data.Time
import           Data.Text.Encoding (decodeUtf8)
import           Text.Digestive
import           Text.Digestive.Snap (runForm)
import           Text.Digestive.Heist
import qualified Data.ByteString.Char8        as BS
import           Control.Monad
import           Heist
import qualified Heist.Interpreted as I

newOCommentForm :: (Monad m) => User -> OverviewCommentType -> UTCTime ->
                   Form Text m OverviewComment
newOCommentForm formUser ocType t =
  OverviewComment
  <$> "poster"    .: choice posterOpts Nothing
  <*> "prose"     .: check "Not a valid entry" (not . T.null) (text Nothing)
  <*> ocVoteVal
  <*> pure []
  <*> pure t
  <*> pure []
  where
    posterOpts = [(Just (userName formUser), userName formUser)
                 ,(Nothing, "Anonymous")]
    dimOpts = [(Novelty,"Novelty"),(Rigor,"Rigor"),(Coolness,"Coolness")]
    ocVoteVal = case ocType of
      Summary'  -> pure Nothing
      Praise    -> (Just . (,UpVote)  ) <$> critiqueVoteVal
      Criticism -> (Just . (,DownVote)) <$> critiqueVoteVal
    critiqueVoteVal = "dimension" .: choice dimOpts Nothing

handleNewOComment :: OverviewCommentType ->
                     Handler App (AuthManager App) ()
handleNewOComment commentType = do
  userMap <- query QueryAllUsers
  docMap  <- query QueryAllDocs
  pId'    <- getParam "paperid"
  cId'    <- getParam "commentid"
  let oldComment' = join $ join $ 
                    liftA2 (\dId cId -> (Map.lookup cId . docOComments) <$>
                                        (Map.lookup dId docMap))
                    (join $ readMay . BS.unpack <$> pId' :: (Maybe DocumentId))
                    (join $ readMay . BS.unpack <$> cId' :: Maybe OverviewCommentId)
  authUser' <- currentUser
  t         <- liftIO $ getCurrentTime
  case join $ readMay . T.unpack . decodeUtf8 <$> pId' of
    Nothing -> writeText "paperid error" -- TODO proper error message
    Just pId ->
      case join $ (Map.lookup <$> (userLogin <$> authUser') <*> pure userMap) of
        Nothing -> writeText $ "handleNewOComment - didn't find user in database"
        Just user -> do
          (vw,rs) <- runForm "newOCommentForm" $ newOCommentForm user commentType t
          case rs of
            Just comment -> do
              let user' = maybe Nothing (const $ Just user) (ocPoster comment) -- TODO what's this?
              _ <- update $ AddOComment user' pId comment (join $ readMay . BS.unpack <$> cId')
              redirect . BS.pack $ "/view_article?paperid=" ++ show pId
            Nothing -> do
              case ( ((join $ ocPoster <$> oldComment') == Just (userName user)) ||
                     (oldComment' == Nothing) ) of
                True -> 
                  heistLocal (bindDigestiveSplices vw) $
                  renderWithSplices "new_o_comment"
                  (oCommentFormSplices commentType oldComment' (userName user))
                False ->
                  writeText "Author editor name mismatch"

oCommentFormSplices :: Monad m => OverviewCommentType -> Maybe OverviewComment -> UserName ->
                       Splices (I.Splice m)
oCommentFormSplices commentType oldComment' uname = do
  when (commentType == Summary') ("reBlock" ## I.textSplice "")
  "poster" ## I.textSplice (maybe "TEST" (\oc -> maybe "Anonymous" id (ocPoster oc)) oldComment')
  "posterDisabled" ## I.textSplice (maybe "" (const "disabled") oldComment')
  "value" ## I.textSplice (maybe "Novelty"
                           (\oc -> maybe "IMPOSSIBLE" (T.pack . show . fst) (ocVote oc))
                           oldComment')
  "prose" ## I.textSplice (maybe "" ocText oldComment')
