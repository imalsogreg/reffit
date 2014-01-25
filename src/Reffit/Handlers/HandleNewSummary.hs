{-# LANGUAGE OverloadedStrings #-}

module Reffit.Handlers.HandleNewSummary(
--  newSummaryView,
--  newSummaryForm,
--  handleNewSummary
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
import           Data.Monoid
import           Control.Monad.Trans
import qualified Data.Map                     as Map
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Data.Time
import           Data.Text.Encoding (decodeUtf8)
import           Text.Digestive
import           Text.Digestive.Blaze.Html5
import           Heist
import qualified Heist.Interpreted            as I
import           Application
import qualified Text.Blaze.Html5             as H
import           Text.Digestive.Snap (runForm)
import           Text.Digestive.Heist
import qualified Data.ByteString.Char8        as BS
import           Control.Monad

{-
newSummaryForm :: (Monad m) => User -> UTCTime -> Bool -> Form Text m Summary
newSummaryForm formUser t isSummary =
  Summary
  <$> "poster" .: choice posterOpts Nothing
  <*> "prose"  .: check "Not a valid summary" (not . T.null) (text Nothing)
  <*> pure []
  <*> pure t
  where
    posterOpts = [(Just (userName formUser), userName formUser)
                 ,(Nothing,"Anonymous")]

newSummaryView :: View H.Html -> H.Html
newSummaryView view = do
  label       "poster" view "Post as: "
  inputSelect "poster" view

  errorList "prose" view
  label     "prose" view "Article Summary"
  inputText "prose" view

handleNewSummary :: Handler App (AuthManager App) ()
handleNewSummary = do
  userMap   <- query QueryAllUsers
  pId'      <- getParam "paperid"
  authUser' <- currentUser
  t         <- liftIO $ getCurrentTime
  case join $ readMay . T.unpack . decodeUtf8 <$> pId' of
    Nothing -> writeText "paperid error" --TODO proper error message
    Just pId ->
      case join (Map.lookup <$> (userLogin <$> authUser') <*> pure userMap) of
        Nothing -> writeBS "Couldn't find user in database"
        Just user -> do
          (vw,rs) <- runForm "newSummaryForm" $ newSummaryForm user t
          case rs of
            Just summary -> do
              let user' = maybe Nothing (const $ Just user) (summaryPoster summary)
              _ <- update $ AddSummary user' pId summary
              --let a = sId :: SummaryId  -- TODO: Must vote for
              -- Vote for own summary     -- user's summary automatically
              redirect . BS.pack $ "/view_article/" ++ show pId
              -- return $ Just sId --TODO: This doesn't work.
            Nothing -> do
              heistLocal (bindDigestiveSplices vw) $ render "new_summary"
-}
