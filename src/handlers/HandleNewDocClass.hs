{-# LANGUAGE OverloadedStrings #-}

module HandleNewDocClass(
  newDocClassView,
  newDocClassForm,
  handleNewDocClass
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

newDocClassForm :: (Monad m) => Form Text m DocClass
newDocClassForm =
  DocClass
  <$> "docClass"   .: check "Not a valid doc class name." checkDocClass (text Nothing)
    where
      checkDocClass t = all id [T.length t < 20
                               ,(length $ T.words t) < 3
                               ]

newDocClassView :: View H.Html -> H.Html
newDocClassView view = do
  errorList   "docClass" view
  label       "docClass" view "Document Class Name: "
  inputSelect "docClass" view

------------------------------------------------------------------------------
-- | Handles Document Class submission
handleNewDocClass :: Handler App (AuthManager App) ()
handleNewDocClass = handleForm 
  where
   handleForm = do
     -- TODO - check user credentials.  Don't let just anyone create
     -- a new doc class (we need badges)
     userMap <- query QueryAllUsers 
     authUser' <- currentUser
     case (Map.lookup <$> (userLogin <$> authUser') <*> pure userMap) of
       Nothing -> writeText "Error - authUser not in app user database"
       Just Nothing -> writeText "Error - justNothing, I'm not sure how you'd get this."
       Just (Just user)  -> do
         (vw,rs) <- runForm "new_doc_class_form" $ newDocClassForm
         case rs of 
           Just dc -> do --TODO add the actual paper, not this test paper.
             _ <- update $ AddDocClass dc
--             redirect "/" -- TODO: redirect to the new page for that paper
             writeText . (T.append "Got Doc Class: " ) . T.pack . show $ dc 
           Nothing -> do
             heistLocal (bindDigestiveSplices vw) $ render "_new_doc_class"
