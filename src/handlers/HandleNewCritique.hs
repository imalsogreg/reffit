{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module HandleNewCritique(
--  newCritiqueView, 
--  newCritiqueForm,
--  handleNewCritique,
  newOCommentView,
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
import           Snap.Snaplet.AcidState (Update, Query, Acid,
                                         HasAcid (getAcidStore),
                                         makeAcidic,
                                         update,query,acidInit)
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
import           Text.Digestive.Blaze.Html5
import qualified Text.Blaze.Html5             as H
import           Text.Digestive.Snap (runForm)
import           Text.Digestive.Heist  
import qualified Data.ByteString.Char8        as BS
import           Control.Monad
import           Heist
import           Snap.Snaplet.Heist
import qualified Heist.Interpreted as I

newOCommentForm :: (Monad m) => User -> OverviewCommentType -> UTCTime -> Form Text m OverviewComment
newOCommentForm formUser ocType t =
  OverviewComment
  <$> "poster"    .: choice posterOpts Nothing
  <*> "prose"     .: check "Not a valid entry" (not . T.null) (text Nothing)
  <*> ocVoteVal
  <*> pure []
  <*> pure t 
  where
    posterOpts = [(Just (userName formUser), userName formUser)
                 ,(Nothing, "Anonymous")]
    dimOpts = [(Novelty,"Novelty"),(Rigor,"Rigor"),(Coolness,"Coolness")]
    ocVoteVal = case ocType of
      Summary'  -> pure Nothing
      Praise    -> (Just . (,UpVote)  ) <$> critiqueVoteVal
      Criticism -> (Just . (,DownVote)) <$> critiqueVoteVal
    critiqueVoteVal = "dimension" .: choice dimOpts Nothing
    
newCritiqueForm :: (Monad m) => User -> UpDownVote -> UTCTime -> Form Text m Critique
newCritiqueForm formUser critValue t =
  Critique
  <$> "prose"     .: check "Not a valid critique" (not . T.null) (text Nothing)
  <*> "poster"    .: choice posterOpts Nothing
  <*> "dimension" .: choice dimOpts Nothing
  <*> pure critValue
  <*> pure []
  <*> pure t
  where 
    posterOpts = [(Just (userName formUser), userName formUser)
                 ,(Nothing,"Anonymous")]
    dimOpts = [(Novelty,"Novelty"),(Rigor,"Rigor"),(Coolness,"Coolness")]


newCritiqueView :: View H.Html -> H.Html
newCritiqueView view = do
  label       "poster" view "Post as: "
  inputSelect "poster" view
  
  label       "dimension" view "Critique dimension: "
  inputSelect "dimension" view 
  
  errorList "prose" view
  label     "prose" view "Article Critique"
  inputText "prose" view

-- I write the form in a .tpl rather than digestive-functors
-- So that I can style it with twitter bootstrap
newOCommentView :: View H.Html -> H.Html
newOCommentView = undefined

{-
handleNewCritique :: UpDownVote -> Handler App (AuthManager App) ()  
handleNewCritique critVal = do 
  userMap   <- query QueryAllUsers
  pId'      <- getParam "paperid"
  authUser' <- currentUser
  t         <- liftIO $ getCurrentTime
  case join $ readMay . T.unpack . decodeUtf8 <$> pId' of  
    Nothing -> writeText "paperid error" --TODO proper error message
    Just pId -> 
      case join $ (Map.lookup <$> (userLogin <$> authUser') <*> pure userMap) of
        Nothing -> writeBS "didn't find user in database"
        Just user -> do
          (vw,rs) <- runForm "newCritiqueForm" $ newCritiqueForm user critVal t
          case rs of 
            Just critique -> do
              let user' = maybe Nothing (const $ Just user) (critiquePoster critique)
              _ <- update $ AddCritique user' pId critique 
              --let a = sId :: SummaryId  -- TODO: Must vote for
              -- Vote for own summary     -- user's summary automatically
              redirect . BS.pack $ "/view_article/" ++ show pId
            Nothing -> do 
              heistLocal (bindDigestiveSplices vw) $ render "new_critique"
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
        Nothing -> do
          method GET handleNoUser <|> method POST handleCatchLoggedOut
            where 
              handleNoUser = writeText "handleNewOComment: User not found in database."
              handleCatchLoggedOut = do
                proseText <- fmap decodeUtf8 <$> getParam "prose" 
                renderWithSplices "caught_logout" (caughtLoggedOutSplices proseText)
        Just user -> do
          method GET handleRenderForm <|> method POST handlePostForm
            where 
              handleRenderForm = do
                (vw,rs) <- runForm "newOCommentForm" $ newOCommentForm user commentType t
                case rs of 
                  Nothing -> do
                    heistLocal (bindDigestiveSplices vw) $ 
                      renderWithSplices "new_o_comment" (oCommentFormSplices commentType)
                  Just _ -> writeText "server error - impossible case in handleNewOComment handleRenderForm"
              handlePostForm = do
                (_,rs) <- runForm "newOCommentForm" $ newOCommentForm user commentType t -- What is this?
                case rs of
                  Just comment -> do
                    let user' = maybe Nothing (const $ Just user) (ocPoster comment)
                    _ <- update $ AddOComment user' pId comment 
                    redirect . BS.pack $ "/view_article/" ++ show pId
                  Nothing -> writeText "server error - impossible case in handleNewOComment handlePostForm"


oCommentFormSplices :: Monad m => OverviewCommentType -> Splices (I.Splice m)
oCommentFormSplices Summary' = "reBlock" ## I.textSplice ""
oCommentFormSplices _ = return ()

caughtLoggedOutSplices :: Monad m => Maybe Text -> Splices (I.Splice m)
caughtLoggedOutSplices Nothing  = "pBlock" ## I.textSplice ""
caughtLoggedOutSplices (Just t) = "prose"  ## I.textSplice t