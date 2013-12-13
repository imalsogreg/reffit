{-# LANGUAGE OverloadedStrings #-}

module HandleNewSummary(
  newSummaryView,
  newSummaryForm,
  handleNewSummary
  )
where

import           Reffit.Types
import           Reffit.AcidTypes
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
import qualified Data.Map                     as Map
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Text.Digestive
import           Text.Digestive.Blaze.Html5
import           Heist 
import qualified Heist.Interpreted            as I
import           Application
import qualified Text.Blaze.Html5             as H
import           Text.Digestive.Snap (runForm)
import           Text.Digestive.Heist  

newSummaryForm :: (Monad m) => User -> Form Text m Summary
newSummaryForm formUser =
  Summary
  <$> "poster" .: choice posterOpts Nothing
  <*> "prose"  .: check "Not a valid summary" (not . T.null) (text Nothing)
  <*> pure [] 
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
  userMap <- query QueryAllUsers
  docs    <- query QueryAllDocs
  ft      <- query QueryAllFieldTags
  authUser' <- currentUser
  case (Map.lookup <$> (userLogin <$> authUser') <*> pure userMap) of
    Just (Just user) -> do
      (vw,rs) <- runForm "newSummaryForm" $ newSummaryForm user
      case rs of
        Just summary -> do
          _ <- update $ AddSummary --TODO Come back here