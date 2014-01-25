{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Reffit.Handlers.HandleNewOComment(
  newOCommentForm,
  handleNewOComment
  )
where

import           Reffit.Types
import           Reffit.AcidTypes
import           Reffit.OverviewComment
import           Reffit.User
import           Reffit.Handlers.Helpers

import           Application
import           Snap.Snaplet.AcidState (update)
import           Snap.Core
import           Snap.Snaplet(Handler)
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Auth
import           Control.Applicative
import           Control.Monad.Trans
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Data.Time
import           Text.Digestive
import           Text.Digestive.Snap (runForm)
import           Text.Digestive.Heist
import qualified Data.ByteString.Char8        as BS
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

handleNewOComment :: OverviewCommentType -> Handler App (AuthManager App) ()
handleNewOComment commentType = do
  pId' <- getIntegralParam "paperid"
  u'   <- currentReffitUser
  t    <- liftIO $ getCurrentTime
  case pId' of
    Nothing -> writeText "paperid error" -- TODO proper error message
    Just pId ->
      case u' of
        Nothing -> writeText $
                   "handleNewOComment - didn't find user in database"
        Just user -> do
          (vw,rs) <- runForm "newOCommentForm" $
                     newOCommentForm user commentType t -- What is this?
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
