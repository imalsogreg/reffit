{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module HandleNewCritique(
--  newOCommentView,
  newOCommentForm,
  handleNewOComment
  )
where

import           Reffit.Types
import           Reffit.AcidTypes
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
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Data.Time
import           Data.Text.Encoding (decodeUtf8)
import           Text.Digestive
import qualified Text.Blaze.Html5             as H
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


-- I write the form in a .tpl rather than digestive-functors
-- So that I can style it with twitter bootstrap
{-
newOCommentView :: View H.Html -> H.Html
newOCommentView = undefined
-}

handleNewOComment :: OverviewCommentType -> Handler App (AuthManager App) ()
handleNewOComment commentType = do
  userMap <- query QueryAllUsers
  pId'    <- getParam "paperid"
  authUser' <- currentUser
  t         <- liftIO $ getCurrentTime
  case join $ readMay . T.unpack . decodeUtf8 <$> pId' of
    Nothing -> writeText "paperid error" -- TODO proper error message
    Just pId ->
      case join $ (Map.lookup <$> (userLogin <$> authUser') <*> pure userMap) of
        Nothing -> writeText $ "handleNewOComment - didn't find user in database"
        Just user -> do
          (vw,rs) <- runForm "newOCommentForm" $ newOCommentForm user commentType t -- What is this?
          case rs of
            Just comment -> do
              let user' = maybe Nothing (const $ Just user) (ocPoster comment)
              _ <- update $ AddOComment user' pId comment
              redirect . BS.pack $ "/view_article/" ++ show pId
            Nothing -> do
              heistLocal (bindDigestiveSplices vw) $
                renderWithSplices "new_o_comment" (oCommentFormSplices commentType)

oCommentFormSplices :: Monad m => OverviewCommentType -> Splices (I.Splice m)
oCommentFormSplices Summary' = do
  "reBlock" ## I.textSplice ""
oCommentFormSplices _ = return ()
